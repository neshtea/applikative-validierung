module Validation.Main (main) where

data UserRole = ADMIN | DEVELOPER | REPORTER deriving (Show, Eq)

data User = User { userName  :: String
                 , userEmail :: String
                 , userRole  :: UserRole }
            deriving (Show, Eq)

-- | Construct a new `User` from a name and an email iff they are both
-- valid.
makeUser :: String -> String -> UserRole -> Either Errors User
makeUser name email role =
  let nameOk = not $ null name
      emailOk = '@' `elem` email
      rightUser = Right $ User name email role
  in
    if nameOk && emailOk
    then rightUser
    else
      if nameOk && not emailOk
      then Left ["not a valid email"]
      else
        if not nameOk && emailOk
        then Left ["not a nonempty string"]
        else Left ["not a valid email", "not a nonempty string"]

type Errors = [String]
data Validation c = Ok c
                  | Fail Errors deriving (Show, Eq)

-- | Validate that `candidate` is not empty.
validateNonemptyString :: String -> Validation String
validateNonemptyString candidate
  | null candidate = Fail ["not a nonempty string"]
  | otherwise      = Ok candidate

validateUserName :: String -> Validation String
validateUserName = validateNonemptyString

-- | Validate that a string represents an email.
validateEmail :: String -> Validation String
validateEmail candidate
  | '@' `elem` candidate = Ok candidate
  | otherwise            = Fail ["not an email"]

-- | Validate that a `candidate` represents a `UserRole`.
validateUserRole :: String -> Validation UserRole
validateUserRole candidate
  | candidate == "ADMIN"     = Ok ADMIN
  | candidate == "DEVELOPER" = Ok DEVELOPER
  | candidate == "REPORTER"  = Ok REPORTER
  | otherwise                = Fail ["not a role"]

-- | Wie zwei Fehler aneinandergehängt werden, ist nicht weiter
-- kompliziert.  Wir nehmen einfach die jeweiligen Fehler und hängen
-- diese per Listenconcatenation (++) aneinander.
combineFails :: Validation c -> Validation c -> Validation c
combineFails (Fail es) (Fail fs) = Fail $ es ++ fs
combineFails _ _ = Fail []

-- combineValidations :: Validation a -> Validation a -> Validation a
combineValidations :: Validation (a -> b) -> Validation a -> Validation b
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
combineValidations (Fail es) _ = Fail es
combineValidations _ (Fail es) = Fail es
combineValidations (Ok f) v = applyOkFunctionToValidation f v

applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
applyOkFunctionToValidation _ (Fail es) = Fail es
applyOkFunctionToValidation f (Ok c) = Ok $ f c

makeValidation :: a -> Validation a
makeValidation x = Ok x

-- Dann sollte doch auch so etwas gehen

-- makeValidation User
--   `combineValidations` (validateNonemptyString "Marco")
--   `combineValidations` (validateEmail "marco.schneider@active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")

-- Das funktioniert?  Schauen wir uns an, ob die Fehelr 

-- makeValidation User
--   `combineValidations` (validateNonemptyString "")
--   `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")

-- Tatsache!  Wie kann das sein?  Dazu schauen wir uns einmal
-- konzentriert eine kleine Reduktion an.  Wir wissen, dass `User`
-- eine Funktion mit drei argumenten ist.  `makeValidation` "hebt" nur
-- den `User` Konstruktor auf die Ebene der Validierung.  Wir erhalten also im ersten Schritt:

-- (Ok (\name email role -> User name admin role))
--   `combineValidations` (validateNonemptyString "Marco")
--   `combineValidations` (validateEmail "marco.schneider@active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")

-- Soweit, so gut.  Werten wir das erste `combineValidations` aus, sieht das ganze so aus:

-- (Ok (\name email role -> User name admin role))
--   `combineValidations` (Ok "Marco")
--   `combineValidations` (validateEmail "marco.schneider@active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")
-- =>
-- (Ok (\email role -> User "Marco" admin role))
--   `combineValidations` (validateEmail "marco.schneider@active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")

-- Interessant.  Weil wir sowohl links aus auch rechts vom
-- `combineValidations` nur `Ok`s haben, wird die linke Funktion mit
-- dem gebundenen Wert des rechten `Ok` aufgerufen.  Da Haskell
-- netterweise von selbst partielle Funktionsapplikation unternimmt,
-- ist das Resultat `Ok` mit einer Funktion, die ein Argument weniger
-- erwartet und in dessen Body `name` bereits an "Marco" gebunden ist.
-- Und so geht es weiter:

-- (Ok (\email role -> User "Marco" admin role))
--   `combineValidations` "marco.schneider@active-group.de"
--   `combineValidations` (validateUserRole "DEVELOPER")
-- =>
-- (Ok (\role -> User "Marco" "marco.schneider@active.group" role))
--   `combineValidations` (validateUserRole "DEVELOPER")
-- => 
-- (Ok (\role -> User "Marco" "marco.schneider@active.group" role))
--   `combineValidations` DEVELOPER
-- => 
-- (Ok (User "Marco" "marco.schneider@active.group" DEVELOPER))

-- Tatsache, alles Validierungen waren erfolgreich und das Ergebnis
-- enspricht den Erwartungen.  Das war aber, um ehrlich zu sein, nicht
-- unser Ziel: Das eigentliche Ziel war es, unterwegs _alle_ Fehler
-- einzusammeln.  Was würde passieren, wenn die Validierungen
-- fehlschlagen (wir steigen nach dem ersten Erfolg ein):


-- (Ok (\email role -> User "Marco" admin role))
--   `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
--   `combineValidations` (validateUserRole "DEVELOPER")
-- =>
-- (Ok (\email role -> User "Marco" admin role))
--   `combineValidations` (Fail ["not an email"])
--   `combineValidations` (validateUserRole "DEVELOPER")

-- Wir erinnern uns: Wenn links oder rechts (aber nicht auf beiden
-- Seiten) von `combineValidations` ein `Fail` steht, bleibt nur das
-- `Fail` übrig.  Also:

-- (Ok (\email role -> User "Marco" admin role))
--   `combineValidations` (Fail ["not an email"])
--   `combineValidations` (validateUserRole "DEVELOPER")
-- =>
-- (Fail ["not an email"])
--   `combineValidations` (validateUserRole "DEVELOPER")
-- =>
-- (Fail ["not an email"])
--   `combineValidations` (Fail ["not a valid role"])

-- Jetzt stehen nur noch zwei `Fail`s.  Wir wissen: Beide Fehlerlisten werden zu einer kombiniert:

-- (Fail ["not an email" "not a valid roel"])

-- Et voila!  Ein Fehler mit allen Fehlschlägen!

-- Was hat das ganze mit Applikativen Funktoren zu tun?

-- instance Semigroup (Validation a) where
--   (Fail es) <> (Fail fs) = Fail (es <> fs)
--   (Fail es) <> _         = Fail es
--   _         <> (Fail fs) = Fail fs

-- instance Monoid (Validation a) where
--   mempty = Fail []
--   mappend = (<>)

instance Functor Validation where
  fmap = applyOkFunctionToValidation

instance Applicative Validation where
  pure = makeValidation
  v1 <*> v2 = combineValidations v1 v2

main :: IO ()
main = do
  print $ validateNonemptyString "ok"
  print $ validateNonemptyString ""
  print $ validateEmail "marco.schneider@active-group.de"
  print $ validateEmail "marco.schneider.at.active-group.de"
  print $ validateUserRole "ADMIN"
  print $ validateUserRole "DEVELOPER"
  print $ validateUserRole "unknown"

  -- print $ combineValidations (validateNonemptyString "") (validateNonemptyString "a")
  print $ (Ok id) `combineValidations` validateNonemptyString ""
  print $ (Ok id) `combineValidations` validateNonemptyString "marco"
  print $ (Ok id) `combineValidations` validateNonemptyString "marco"

  print $ makeValidation User
    `combineValidations` (validateNonemptyString "Marco")
    `combineValidations` (validateEmail "marco.schneider@active-group.de")
    `combineValidations` (validateUserRole "DEVELOPER")
  print $ makeValidation User
    `combineValidations` (validateNonemptyString "")
    `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
    `combineValidations` (validateUserRole "DEVELOPE")
