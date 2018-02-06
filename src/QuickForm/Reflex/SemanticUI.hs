{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module QuickForm.Reflex.SemanticUI where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Monad (void, join)
import Data.Default
import Data.Foldable (for_, traverse_)
import Data.Maybe (isJust)
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.TypeLits
import Reflex.Dom.SemanticUI hiding (HList(..))
import Reflex.Dom.Core (textInput_input, text)
import QuickForm
import QuickForm.TypeLevel

import qualified Data.Text as T

symbolText' :: forall s. KnownSymbol s => Text
symbolText' = symbolText $ Proxy @s

data FormUI t m (q :: QuickForm) where
  ValidatedUI
    :: (forall b. Dynamic t (Maybe (FormErr (Validated e a q))) -> m b -> m b)
    -> FormUI t m q
    -> FormUI t m (Validated e a q)
  UnvalidatedUI
    -- :: (forall a. Dynamic t (Maybe (FormErr q)) -> m a -> m a)
    :: (forall a. m a -> m a)
    -> FormUI t m q
    -> FormUI t m (Unvalidated a q)
  TextFieldUI
    :: (forall a. (TextInputConfig t -> m a) -> m a)
    -> FormUI t m (Field n Text)
  CheckboxUI
    :: (forall a. (Active t Text -> CheckboxConfig t -> m a) -> m a)
    -> FormUI t m (Field n Bool)
  EnumUI
    :: (forall a. m a -> m a)
    -> FormUI t m (Field n a)
  FormUI
    :: (TList (Map (RunFormM t m) qs) -> m (TList (Map (DynForm t) qs)))
    -> TList (Map (FormUI t m) qs)
    -> FormUI t m (SubForm qs)

class DisplayList qs where
  displayList :: MonadWidget t m => TList (Map (DynErrs t) qs) -> m ()
instance DisplayList '[] where
  displayList Nil = blank
instance (Show (FormErr q), DisplayList qs) => DisplayList (q ': qs) where
  displayList (q :| qs) = do
    display (dynErrs q)
    displayList @qs qs

newtype DynErrs t q = DynErrs { dynErrs :: Dynamic t (FormErr q) }


getErr :: FormErr (Validated e a q) -> Maybe e
getErr (ValidatedErr e _) = e

class SequenceList (as :: [k]) where
  sequenceTList
    :: Applicative m => Proxy as -> (forall a. f a -> m (g a))
    -> TList (Map f as) -> m (TList (Map g as))
instance SequenceList '[] where sequenceTList _ _ Nil = pure Nil
instance SequenceList as => SequenceList (a ': as) where
  sequenceTList _ f (m :| rest)
    = (:|) <$> f m <*> sequenceTList (Proxy @as) f rest


data DynForm t q = DynForm
  { dynFormRaw :: Dynamic t (FormRaw q)
  , dynFormVal :: Dynamic t (ValResult q)
  }

class SequenceDynForm (qs :: [QuickForm]) where
  sequenceDynForm :: Reflex t
    => TList (Map (DynForm t) qs)
    -> ( Dynamic t (FormRaw (SubForm qs))
       , Dynamic t (TList (Map ValResult qs)))

instance SequenceDynForm '[] where
  sequenceDynForm Nil = (pure $ FormRaw Nil, pure Nil)

instance SequenceDynForm qs => SequenceDynForm (q ': qs) where
  sequenceDynForm (DynForm raw val :| ds)
    = let (raws, vals) = sequenceDynForm @qs ds
       in ( (\a (FormRaw b) -> FormRaw $ a :| b) <$> raw <*> raws
          , (\a b -> a :| b) <$> val <*> vals )

--------------------------------------------------------------------------------

instance (Applicative m, SequenceList qs, Default (TList (Map (FormUI t m) qs)))
  => Default (FormUI t m (SubForm qs)) where
    def = FormUI (sequenceTList (Proxy @qs) (runFormM @t @m)) def

instance (MonadWidget t m, KnownSymbol n, Reflex t)
  => Default (FormUI t m (Field n Text)) where
  def = TextFieldUI $ \m -> do
    field def $ do
      el "label" $ text name
      input def $ m $ def & textInputPlaceholder |~ name
    where name = symbolText' @n


instance (MonadWidget t m, KnownSymbol n, Reflex t)
  => Default (FormUI t m (Field n Bool)) where
  def = CheckboxUI $ \m -> do
    field def $ do
      el "label" $ text name
      m (pure name) def
    where name = symbolText' @n

instance {-# OVERLAPS #-} (MonadWidget t m, KnownSymbol n, Reflex t, Bounded a, Enum a, Show a)
  => Default (FormUI t m (Field n a)) where
  def = EnumUI $ \m -> do
    field def $ do
      el "label" $ text name
      m
    where name = symbolText' @n

instance {-# OVERLAPS #-}
  ( Show e, Eq e
  , Default (FormUI t m q)
  , MonadWidget t m
  ) => Default (FormUI t m (Validated e a q)) where
    def = ValidatedUI (validatedWithError def . (fmap . fmap) f) def
      where f :: FormErr (Validated e a q) -> FormErr (Validated (Identity e) a q)
            f (ValidatedErr Nothing q) = ValidatedErr Nothing q
            f (ValidatedErr (Just e) q) = ValidatedErr (Just $ Identity e) q

instance
  ( Show e, Foldable f, Alternative f, Eq (f e)
  , Default (FormUI t m q)
  , MonadWidget t m
  ) => Default (FormUI t m (Validated (f e) a q)) where
    def = ValidatedUI (validatedWithError def) def where

validatedWithError
  :: (MonadWidget t m, Show e, Foldable f)
  => FieldConfig t -> Dynamic t (Maybe (FormErr (Validated (f e) a q)))
  -> m b -> m b
validatedWithError conf dMaybeFormErr child = do
  let dMaybeErr = join . fmap getErr <$> dMaybeFormErr
  field (conf & fieldError .~ Dynamic (isJust <$> dMaybeErr)) $ do
    result <- child
    dyn $ ffor dMaybeErr $ traverse_ $ \es ->
      message (def & messageType |?~ MessageType Error) $
        for_ es $ listItem def . text . tshow
    pure result

validatedWithoutError
  :: (MonadWidget t m, Show e, Foldable f)
  => FieldConfig t -> Dynamic t (Maybe (FormErr (Validated (f e) a q)))
  -> m b -> m b
validatedWithoutError conf dMaybeError child = do
  field (conf & fieldError .~ Dynamic (isJust . join . fmap getErr <$> dMaybeError)) $ do
    result <- child
    pure result

instance Default (FormUI t m q) => Default (FormUI t m (Unvalidated a q)) where
  def = UnvalidatedUI id def

--unvalidatedWithSubError
--  :: (MonadWidget t m, Show e, Foldable f)
--  => FieldConfig t -> Dynamic t (Maybe (FormErr q))
--  -> m b -> m b
--unvalidatedWithSubError conf dMaybeFormErr child = do
--  field (conf & fieldError .~ Dynamic (isJust <$> dMaybeErr)) $ do
--    result <- child
--    dyn $ ffor dMaybeErr $ traverse_ $ \es ->
--      message (def & messageType |?~ MessageType Error) $
--        for_ es $ listItem def . text . tshow
--    pure result

--------------------------------------------------------------------------------

newtype RunFormM t (m :: * -> *) (q :: QuickForm) = RunFormM
  { runFormM :: m (DynForm t q) }

-- | When validation should be performed. Note that validation is also
-- performed on the submit event for 'OnBlur' and 'OnEvent'.
data FormValidation t
  = Continuous
  -- ^ Continuously validate as the user types
  | OnBlur
  -- ^ Validate when a user leaves a field
  | OnSubmit
  -- ^ Validate only when submitting the form
  | OnEvent (Event t ())
  -- ^ Validate on some other event

data FormSettings t = FormSettings
  { _formValidation :: FormValidation t
  }
makeLenses ''FormSettings

instance Default (FormSettings t) where
  def = FormSettings
    { _formValidation = Continuous
    }

runFormUI
  :: forall t m q a. (Wf q, MonadWidget t m, RunForm q)
  => FormSettings t
  -> FormVal q
  -> FormUI t m q
  -> m (Event t (), a)
  -> m (DynForm t q, a)
runFormUI settings f m rest = mdo
  let eReset = domEvent Reset e
  (e, a) <- form' (def & classes |~ "error success") $ mdo
    result <- runFormM $ runFormUI' (Proxy @m) settings submit eReset f m
    (submit, a) <- rest
    pure (result, a)
  pure a

class Wf q => RunForm (q :: QuickForm) where
  runFormUI'
    :: MonadWidget t m
    => Proxy m
    -> FormSettings t
    -> Event t () -- ^ Submit event
    -> Event t () -- ^ Reset event
    -> FormVal q
    -> FormUI t m q
    -> RunFormM t m q

-- | Regular text fields (type: text, password, email...)
instance KnownSymbol n => RunForm (Field n Text) where
  runFormUI' _ FormSettings {..} eSubmit eReset FieldVal (TextFieldUI render)
    = RunFormM $ do
      ti <- render $ \tiConf -> textInput $ tiConf
          & textInputAttrs |~ ("name" =: symbolText' @n)

      let dValue = view textInput_value ti
          eBlur = void $ ffilter not $ updated $ view textInput_hasFocus ti

      val <- holdDyn (Err FieldErr) $ leftmost
        [ Hs . FieldHs <$> case _formValidation of
            Continuous -> updated dValue
            OnBlur -> tagPromptlyDyn dValue (eBlur <> eSubmit)
            OnSubmit -> tagPromptlyDyn dValue eSubmit
            OnEvent e -> tagPromptlyDyn dValue (e <> eSubmit)
        , Err FieldErr <$ eReset ]

      pure $ DynForm (FieldRaw <$> dValue) val

-- | Regular text fields (type: text, password, email...)
instance KnownSymbol n => RunForm (Field n Bool) where
  runFormUI' _ FormSettings {..} eSubmit eReset FieldVal (CheckboxUI render)
    = RunFormM $ do
      cb <- render $ \txt conf -> checkbox txt $ conf
          & attrs |~ ("name" =: symbolText' @n)

      let eInput = view checkboxChange cb
          dValue = view checkboxValue cb
          eBlur = void $ ffilter not $ updated $ view checkboxHasFocus cb

      val <- holdDyn (Err FieldErr) $ leftmost
        [ Hs . FieldHs <$> case _formValidation of
            Continuous -> eInput
            OnBlur -> tagPromptlyDyn dValue (eBlur <> eSubmit)
            OnSubmit -> tagPromptlyDyn dValue eSubmit
            OnEvent e -> tagPromptlyDyn dValue (e <> eSubmit)
        , Err FieldErr <$ eReset ]

      pure $ DynForm (FieldRaw <$> dValue) val

-- | Enum fields
instance {-# OVERLAPS #-} (KnownSymbol n, Enum a, Bounded a, Show a)
  => RunForm (Field n a) where
  runFormUI' _ _ _ _ FieldVal (EnumUI render) = RunFormM $ do
    text $ tshow $ [minBound .. (maxBound :: a)]
    pure $ DynForm (pure $ FieldRaw minBound) (pure $ Hs $ FieldHs minBound)

-- | Enum fields
instance (KnownSymbol n, Enum a, Bounded a, Show a)
  => RunForm (Field n [a]) where
  runFormUI' _ _ _ _ FieldVal (EnumUI render) = RunFormM $ do
    let vals = [minBound .. (maxBound :: a)]
    text $ tshow vals
    pure $ DynForm (pure $ FieldRaw vals) (pure $ Err FieldErr)

instance RunForm q => RunForm (Unvalidated a q) where
  runFormUI' proxy settings eSubmit eReset
    (UnvalidatedVal f subVal) (UnvalidatedUI wrapper subUI) = RunFormM $ do
    DynForm raw val <- wrapper $ runFormM $
      runFormUI' proxy settings eSubmit eReset subVal subUI
    pure $ DynForm
      { dynFormRaw = UnvalidatedRaw <$> raw
      , dynFormVal = runUnvalidated f <$> val
      }

instance (Default (FormErr q), RunForm q)
  => RunForm (Validated e a q) where
    runFormUI' proxy settings eSubmit eReset
      (ValidatedVal f subVal) (ValidatedUI wrapper subUI) = RunFormM $ mdo
      DynForm raw val <- wrapper mErr $ runFormM $
        runFormUI' proxy settings eSubmit eReset subVal subUI

      let res = runValidated f <$> val
          mErr = ffor res valErr

      pure $ DynForm
        { dynFormRaw = ValidatedRaw <$> raw
        , dynFormVal = res
        }

instance
  ( All Wf qs, Unique (Concat (MapNames qs)) ~ 'True
  , SequenceDynForm qs, RunSubForm qs, JoinSubForm qs
  ) => RunForm (SubForm qs) where
    runFormUI' proxy settings eSubmit eReset fs
      (FormUI wrapper fUIs) = RunFormM $ do
      hlist <- wrapper $ runSubForm proxy settings eSubmit eReset fs fUIs
      pure $ (sequenceDynForm @qs hlist) &
        \(raw, val) -> DynForm raw (joinSubForm <$> val)

class RunSubForm (qs :: [QuickForm]) where
  runSubForm
    :: forall t m. MonadWidget t m
    => Proxy m
    -> FormSettings t
    -> Event t ()
    -> Event t ()
    -> FormVal (SubForm qs)
    -> TList (Map (FormUI t m) qs)
    -> TList (Map (RunFormM t m) qs)

instance RunSubForm '[] where
  runSubForm _ _ _ _ (FormVal Nil) Nil = Nil

instance (RunForm q, RunSubForm qs) => RunSubForm (q ': qs) where
  runSubForm proxy settings eSubmit eReset (FormVal (f :| fs)) (ui :| uis) =
    runFormUI' proxy settings eSubmit eReset f ui
      :| runSubForm @qs proxy settings eSubmit eReset (FormVal fs) uis







