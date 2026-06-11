{-# LANGUAGE OverloadedStrings #-}

-- | IAM-like policy evaluation engine.
--
-- This module implements a policy evaluation engine for IAM-like access control
-- in an S3-compatible object storage service. The evaluation follows these rules:
--
-- 1. __Deny overrides__: If any policy explicitly denies the action on the resource,
--    the request is denied regardless of any Allow policies.
-- 2. __Default deny__: If no policy matches, the request is denied.
-- 3. __Allow on match__: If an Allow policy matches and no Deny policy matches,
--    the request is allowed.
--
-- Policy statements consist of an effect ('Allow' or 'Deny'), a list of 'Action's,
-- and a list of 'ResourceARN' patterns.
--
-- Resource ARN patterns support:
--
-- * Exact matches: @\"arn:aws:s3:::my-bucket\/object.txt\"@
-- * Trailing wildcard: @\"arn:aws:s3:::my-bucket\/*\"@
-- * Leading wildcard: @\"*suffix\"@
-- * Middle wildcard: @\"prefix*suffix\"@
-- * Universal wildcard: @\"*\"@
--
-- Only a single @*@ wildcard character per pattern is supported; patterns with
-- multiple @*@ characters are treated as non-matching (deny).
--
-- Actions support a universal wildcard 'S3AllActions' which matches any action.
module S3OSS.Auth.Policy (evaluate) where

import RIO hiding (evaluate)
import Data.List (find)
import S3OSS.Types
import qualified RIO.Text as T

-- | Evaluate policies for a given action and resource.
-- Returns True if the action is allowed.
-- Deny always overrides Allow. Default is deny (no match = False).
evaluate :: [Policy] -> Action -> ResourceARN -> Bool
evaluate policies action resource =
  case find (matchesEffect Deny action resource) policies of
    Just _  -> False  -- explicit deny
    Nothing -> any (matchesEffect Allow action resource) policies

matchesEffect :: Effect -> Action -> ResourceARN -> Policy -> Bool
matchesEffect eff action resource p =
  policyEffect p == eff
  && actionMatches action (policyActions p)
  && resourceMatches resource (policyResources p)

-- | Check if an action is covered by a list of policy actions.
actionMatches :: Action -> [Action] -> Bool
actionMatches _    actions | S3AllActions `elem` actions = True
actionMatches action actions = action `elem` actions

-- | Check if a resource ARN matches patterns in a policy.
--
-- A 'ResourceARN' of @\"*\"@ is a fast-path short-circuit that matches
-- any resource. More specific patterns such as @\"arn:aws:s3:::*\"@ are
-- handled by 'matchARN' rather than this short-circuit.
resourceMatches :: ResourceARN -> [ResourceARN] -> Bool
resourceMatches _ resources | ResourceARN "*" `elem` resources = True
resourceMatches (ResourceARN target) patterns =
  any (matchARN target . unResourceARN) patterns

-- | Match a resource ARN against a pattern with wildcard support.
--
-- The pattern may contain at most one @*@ wildcard character, which matches
-- any sequence of characters (including the empty sequence). Patterns with
-- more than one @*@ are treated as non-matching, producing a safe default
-- (deny) behavior.
--
-- Supported patterns:
--
-- * @\"*\"@ -- matches everything
-- * @\"prefix*\"@ -- trailing wildcard (starts with @prefix@)
-- * @\"*suffix\"@ -- leading wildcard (ends with @suffix@)
-- * @\"prefix*suffix\"@ -- middle wildcard (starts with @prefix@ and ends with @suffix@)
-- * No wildcard -- exact match only
--
-- Note: Patterns like @\"arn:aws:s3:::*\"@ are handled by the trailing-wildcard
-- path: the ARN prefix is matched literally and the trailing @*@ matches any
-- remaining characters, making it functionally equivalent to the @\"*\"@
-- short-circuit in 'resourceMatches'.
matchARN :: Text -> Text -> Bool
matchARN _      "*"    = True
matchARN target pattern =
  case T.split (== '*') pattern of
    [p]           -> target == p                           -- no wildcard
    [pre, suf]
      | T.null pre  -> T.isSuffixOf suf target            -- leading wildcard: *suffix
      | T.null suf  -> T.isPrefixOf pre target            -- trailing wildcard: prefix*
      | otherwise   -> T.isPrefixOf pre target             -- middle wildcard: prefix*suffix
                    && T.isSuffixOf suf target
                    && T.length target >= T.length pre + T.length suf
    _             -> False                                 -- multiple wildcards not supported
