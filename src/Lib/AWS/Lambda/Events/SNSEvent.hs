{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.AWS.Lambda.Events.SNSEvent where

import Data.Map (Map)
import Data.Char
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

data MessageAttribute = MessageAttribute { type', value :: String } deriving (Show, Generic)

data SNS = SNS
  { messageAttributes :: Map String MessageAttribute
  , signingCertUrl, messageId, subject, message
--  , type'
  , unsubscribeUrl, signatureVersion, signature, timestamp, topicArn :: String
  , records :: [SNSRecord]
  } deriving (Show, Generic)

data SNSRecord = SNSRecord
  { sns :: SNS, eventVersion, eventSource, eventSubscriptionArn :: String } deriving (Show, Generic)

instance FromJSON MessageAttribute where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropLastPrime . upperCamelize }

instance FromJSON SNS where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = dropLastPrime . upperCamelize }

instance FromJSON SNSRecord where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = upperCamelize }

upperCamelize :: [Char] -> [Char]
upperCamelize x = (map toUpper $ take 1 x) ++ drop 1 x

dropLastPrime :: [Char] -> [Char]
dropLastPrime x = if (take 1 . reverse $ x) == "'" then reverse . drop 1 . reverse $ x else x

