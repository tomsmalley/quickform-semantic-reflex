{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- For symbolText
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad ((<=<))
import Data.Bifunctor
import Data.Default
import Control.Lens
import Control.Monad (void)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import QuickForm
import QuickForm.TypeLevel
import QuickForm.Reflex.SemanticUI
import Text.Read (readMaybe)
import GHC.Generics (Generic)

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.SemanticUI hiding (HList(..))
import Reflex.Dom.SemanticUI.Warp
import Reflex.Dom.Core (text)

import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

data Name = Name { firstName :: Text, lastName :: Text }

type NameForm = Unvalidated Name
  (SubForm [Field "firstname" Text, Field "lastname" Text])

data State = StateOne | StateTwo deriving (Eq, Ord, Show)
data Country = CountryOne | CountryTwo deriving (Eq, Ord, Show)

data Address = Address
  { addressStreet :: Text
  , addressApt :: Text
  , addressState :: Maybe State
  , addressCountry :: Country
  } deriving (Eq, Ord, Show)
data AddressError = EmptyStreet | EmptyApt

mkAddress
  :: TList '[Text, Text, Maybe State, Country] -> Either [AddressError] Address
mkAddress (addressStreet :| addressApt :| addressState :| addressCountry :| Nil)
  = case catMaybes [ notNull addressApt EmptyApt
                   , notNull addressStreet EmptyStreet
                   ] of
    [] -> Right $ Address {..}
    es -> Left es
    where notNull t e = if T.null t then Just e else Nothing

type AddressForm = Validated Address AddressError
  (SubForm [ Field "streetaddress" Text
           , Field "apt" Text
           , Field "state" State
           , Field "country" Country ])

--validateAddressForm :: FormVal AddressForm
--validateAddressForm = ValidatedVal (\(FormHs t) -> mkAddress t) def

argsFromList :: ApplyTList (Map FormHs qs) f
             => f -> FormHs (SubForm qs) -> Return (Map FormHs qs) f
argsFromList f (FormHs l) = applyTList l f

class ApplyTList as a where
  type Return as a
  applyTList :: TList as -> a -> Return as a

instance ApplyTList '[] a where
  type Return '[] a = a
  applyTList Nil a = a

instance ApplyTList as b => ApplyTList (a ': as) (a -> b) where
  type Return (a ': as) (a -> b) = Return as b
  applyTList (a :| as) f = applyTList as $ f a

--------------------------------------------------------------------------------
-- Card

data Card = Card
  { cardType :: CardType
  , cardNumber :: CardNumber
  , cardCVC :: CVC
  , cardExpiry :: (Month, Year)
  } deriving (Eq, Show)

data CardType = Visa | Mastercard | Amex deriving (Eq, Enum, Read, Show)

data CardNumber = CardNumber Text deriving (Eq, Show)
data CardNumberError
  = CardNumberTooShort
  | CardNumberTooLong
  | CardNumberNotDigits
  deriving (Eq, Ord, Show)

mkCardNumber :: Text -> Either [CardNumberError] CardNumber
mkCardNumber t = case catMaybes [areDigits, lengthCheck] of
  [] -> Right $ CardNumber t
  es -> Left es
  where
    str = T.unpack t
    areDigits = if all isDigit str then Nothing else Just CardNumberNotDigits
    lengthCheck
      | length str > 16 = Just CardNumberTooLong
      | length str < 16 = Just CardNumberTooShort
      | otherwise = Nothing

data CVC = CVC Text deriving (Eq, Show)
data CVCError
  = CVCNotDigits
  | CVCNotEnoughDigits
  | CVCTooManyDigits
  deriving (Eq, Ord, Show)

mkCVC :: Text -> Either [CVCError] CVC
mkCVC t = case catMaybes [areDigits, lengthCheck] of
  [] -> Right $ CVC t
  es -> Left es
  where
    str = T.unpack t
    areDigits = if all isDigit str then Nothing else Just CVCNotDigits
    lengthCheck
      | length str > 3 = Just CVCTooManyDigits
      | length str < 3 = Just CVCNotEnoughDigits
      | otherwise = Nothing

newtype Year = Year { getYear :: Int } deriving (Eq, Show)
data YearError
  = YearIsInPast
  | YearIsTooFarInFuture
  | YearIsNotInteger
  deriving (Eq, Ord, Show, Generic)

mkYear :: Text -> Either YearError Year
mkYear t = case readMaybe $ T.unpack t of
  Nothing -> Left YearIsNotInteger
  Just i
    | i < 2018 -> Left YearIsInPast
    | i >= 2048 -> Left YearIsTooFarInFuture
    | otherwise -> Right $ Year i

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Show, Enum, Bounded)
deriveJSON defaultOptions ''Month
--data MonthError = InvalidMonth deriving (Eq, Ord, Show)
--mkMonth :: Int -> Either MonthError Month
--mkMonth i
--  | i >= 1 && i <= 12 = Right $ Month i
--  | otherwise = Left InvalidMonth

--mkMonth :: Text -> Either MonthError Month
--mkMonth t = case readMaybe $ T.unpack t of
--  Nothing -> Left InvalidMonth
--  Just i
--    | i >= 1 && i <= 12 -> Right $ Month i
--    | otherwise -> Left InvalidMonth

type CardForm = Unvalidated Card (SubForm CardFormFields)
type CardFormFields = [CardNumberField, CVCField, MonthField, YearField]
--  (CardTypeField :+: CardNumberField :+: CVCField :+: MonthField :+: YearField)

type CardTypeField = Field "cardtype" CardType
type CardNumberField
  = Validated [CardNumberError] CardNumber (Field "cardnumber" Text)
type CVCField = Validated [CVCError] CVC (Field "cvc" Text)
--type MonthField = Validated MonthError Month (Field "expmonth" (EnumField Int))
type MonthField = Field "expmonth" Month
--type MonthField = Validated [MonthError] Month (Field "expiry-month" Text)
type YearField = Validated (Maybe YearError) Year (Field "expiry-year" Text)

validateYear :: FormVal YearField
validateYear = ValidatedVal (first pure . mkYear . getHs) FieldVal
validateMonth :: FormVal MonthField
validateMonth = FieldVal -- ValidatedVal (first pure . mkMonth . getHs) FieldVal
validateCardNumber :: FormVal CardNumberField
validateCardNumber = ValidatedVal (mkCardNumber . getHs) FieldVal
validateCVC :: FormVal CVCField
validateCVC = ValidatedVal (mkCVC . getHs) FieldVal
validateCardForm :: FormVal CardForm
validateCardForm = UnvalidatedVal val $ FormVal $
  validateCardNumber :| validateCVC :| validateMonth :| validateYear :| Nil
  where
    val :: FormHs (SubForm [CardNumberField, CVCField, MonthField, YearField])
        -> Card
    val (FormHs (ValidatedHs cardNumber :| ValidatedHs cardCVC
              :| FieldHs month :| ValidatedHs year :| Nil)) = Card {..}
      where
        cardExpiry = (month, year)
        cardType = Visa

cardNumberUI
  :: forall t m. MonadWidget t m => FormUI t m CardNumberField
cardNumberUI = ValidatedUI (validatedWithoutError valConf) $ TextFieldUI
  $ \ti -> do
  el "label" $ text "Card Number"
  input def $ ti $ def & textInputPlaceholder |~ "Card #"
    where valConf = def & classes |~ "seven wide"

cvcUI :: MonadWidget t m => FormUI t m CVCField
cvcUI = ValidatedUI (validatedWithoutError valConf) $ TextFieldUI $ \ti -> do
  el "label" $ text "CVC"
  input (def & inputIcon |?~ RightIcon) $ do
    icon "lock" def
    ti $ def & textInputPlaceholder |~ "CVC"
  where valConf = def & classes |~ "three wide"

yearUI :: MonadWidget t m => FormUI t m YearField
yearUI = ValidatedUI (validatedWithoutError def) $ TextFieldUI $ \m -> do
  field def $ input def $ m $ def & textInputPlaceholder |~ "Year"

monthUI :: MonadWidget t m => FormUI t m MonthField
monthUI = def -- EnumUI $ \m -> do
--  field def $ input def $ m

runCardForm :: MonadWidget t m => FormUI t m CardForm
runCardForm = UnvalidatedUI id runCardFields
  where
    runCardFields = FormUI runCardFormFields subfields
    subfields
      = cardNumberUI :| cvcUI :| monthUI :| yearUI :| Nil

    runCardFormFields (cardNoM :| cvcM :| monthM :| yearM :| Nil) = do

      result <- divClass "fields" $ do

        card <- runFormM cardNoM
        cvc <- runFormM cvcM

        (month, year) <- field (def & classes |~ "six wide") $ do
          el "label" $ text "Expiration"
          divClass "two fields" $ do
            month <- runFormM monthM
            year <- runFormM yearM
            pure (month, year)

        pure $ card :| cvc :| month :| year :| Nil

      --displayList @CardFormFields result

      pure result


--------------------------------------------------------------------------------
-- Contacts

data Contacts = Jerry | Julie

type ContactsF = Field "contacts" Contacts
type ReceiptF = Field "receipt" Text

data Accepted = Accepted deriving (Eq, Show)
data NotAccepted = NotAccepted deriving (Eq, Show)

mkAccepted :: Bool -> Either NotAccepted Accepted
mkAccepted b = if b then Right Accepted else Left NotAccepted

valAccepted :: FormVal AcceptTermsField
valAccepted = ValidatedVal (mkAccepted . getHs) def

type AcceptTermsField = Validated NotAccepted Accepted (Field "accept-terms" Bool)

type ExampleForm = SubForm '[ CardForm, AcceptTermsField ]

valExampleForm :: FormVal ExampleForm
valExampleForm = FormVal $ validateCardForm :| valAccepted :| Nil

submitButton
  :: MonadWidget t m
  => Dynamic t Bool -> ButtonConfig t m -> m a -> m (Event t ())
submitButton isLoading conf = button conf' . void
  where
    conf' = conf
      & buttonType .~ SubmitButton
      & buttonEmphasis |?~ Primary
      & buttonLoading .~ Dynamic isLoading

formTest :: MonadWidget t m => m ()
formTest = do

  DynForm raw val <- container def $ segment def $ mdo
    let conf = def & formValidation .~ OnBlur -- OnEvent submit
    (result, ()) <- runFormUI conf valExampleForm def $ do
      button (def & buttonType .~ ResetButton) $ text "Reset"
      submit <- submitButton (pure False) def $ text "Submit"
      pure (submit, ())
    pure result

  container def $ segment def $ display raw
  container def $ segment def $ display val
  container def $ segment def $ do

    dyn $ ffor raw $ \raw -> do
      let value = Aeson.toJSON raw
          parsed = Aeson.fromJSON value

      paragraph $ text $ tshow value
      paragraph $ text $ tshow parsed
      paragraph $ text $ tshow $ parsed == Aeson.Success raw

      let encoded = Aeson.encode raw
          decoded = Aeson.eitherDecode encoded

      paragraph $ text $ TE.decodeUtf8 $ LBS.toStrict encoded
      paragraph $ text $ tshow decoded
      paragraph $ text $ tshow $ decoded == Right raw

  pure ()

jsmMain :: JSM ()
jsmMain = mainWidget formTest

main :: IO ()
main = server 9090 "" jsmMain Nothing

