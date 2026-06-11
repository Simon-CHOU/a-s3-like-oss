{-# LANGUAGE OverloadedStrings #-}

-- | IAM-like policy evaluation engine.
module S3OSS.Auth.Policy where

import RIO
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
resourceMatches :: ResourceARN -> [ResourceARN] -> Bool
resourceMatches _ resources | ResourceARN "*" `elem` resources = True
resourceMatches (ResourceARN target) patterns =
  any (matchARN target . unResourceARN) patterns

-- | Simple ARN matching with wildcard support.
matchARN :: Text -> Text -> Bool
matchARN _      "*"       = True
matchARN target pattern
  | "*" `T.isSuffixOf` pattern =
      let prefix = T.dropEnd 1 pattern
      in prefix `T.isPrefixOf` target
  | otherwise = target == pattern
