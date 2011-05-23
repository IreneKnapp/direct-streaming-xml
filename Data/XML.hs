module Data.XML where

import Text.ParserCombinators.Parsec
import Monad (join)
import Numeric (readDec, readHex)
import Data.Char
import Data.Maybe

--import Text.ParserCombinators.Parsec.Error

data Tag = Tag Name [Attribute] [Content]
data XMLDeclaration = XMLDeclaration [Attribute]
                    | XMLStylesheetDeclaration [Attribute]
data Name = Name String String deriving (Eq)
data Attribute = Attribute Name String
data Content = Text String | Subtag Tag


instance Show Tag where
	show tag@(Tag _ _ []) = showEmpty tag
	show tag@(Tag _ _ content) = (showOpen tag) ++ (show content) ++ (showClose tag)

showEmpty :: Tag -> String
showEmpty (Tag name attributes _) = "<" ++ (show name) ++ (show attributes) ++ "/>"

showOpen :: Tag -> String
showOpen (Tag name attributes _) = "<" ++ (show name) ++ (show attributes) ++ ">"

showClose :: Tag -> String
showClose (Tag name _ _) = "</" ++ (show name) ++ ">"

instance Show XMLDeclaration where
	show (XMLDeclaration attributes) = "<?xml" ++ (show attributes) ++ "?>"
        show (XMLStylesheetDeclaration attributes)
            = "<?xml-stylesheet" ++ (show attributes) ++ "?>"

instance Show Name where
	show (Name "" proper) = proper
	show (Name namespace proper) = namespace ++ ":" ++ proper

instance Show Attribute where
	show (Attribute name value) = (show name) ++ "=\"" ++ value ++ "\""
	showList (a:rest) = (++) $ " " ++ (show a) ++ (show rest)
	showList [] = (++) ""

instance Show Content where
	show (Text text) = escapeString text
	show (Subtag tag) = show tag
	showList (a:rest) = (++) $ (show a) ++ (show rest)
	showList [] = (++) ""

escapeString :: String -> String
escapeString [] = []
escapeString ('<':rest) = concat ["&#x3C;", escapeString rest]
escapeString ('>':rest) = concat ["&#x3E;", escapeString rest]
escapeString ('&':rest) = concat ["&#x26;", escapeString rest]
escapeString (c:rest) = c : escapeString rest


isXmlNameStartChar :: Char -> Bool
isXmlNameStartChar c =
	let
		codepoint = ord c
		inRange (a, b) = a <= codepoint && codepoint <= b
		in isLetter c || c == '_' || any inRange xmlNameStartCharRanges

isXmlNameChar :: Char -> Bool
isXmlNameChar c =
	let
		codepoint = ord c
		inRange (a, b) = a <= codepoint && codepoint <= b
		in isLetter c || isDigit c || c == '-' || c == '.' || c == (chr 0xB7)
			|| any inRange xmlNameCharRanges

xmlNameStartCharRanges :: [(Int, Int)]
xmlNameStartCharRanges = [
	(0xC0, 0xD6), (0xD8, 0xF6), (0xF8, 0x2FF), (0x370, 0x37D), (0x37F, 0x1FFF), (0x200C, 0x200D),
	(0x2070, 0x218F), (0x2C00, 0x2FEF), (0x3001, 0xD7FF), (0xF900, 0xFDCF), (0xFDF0, 0xFFFD),
	(0x10000, 0xEFFFF) ]

xmlNameCharRanges :: [(Int, Int)]
xmlNameCharRanges = xmlNameStartCharRanges ++ [(0x0300, 0x036F), (0x203F, 0x2040)]

xmlNameStartChar :: Parser Char
xmlNameStartChar = satisfy isXmlNameStartChar
	<?> "name start character"
xmlNameChar :: Parser Char
xmlNameChar = satisfy isXmlNameChar
	<?> "name character"

xmlNameComponent :: Parser String
xmlNameComponent = do
		a <- xmlNameStartChar
		b <- many xmlNameChar
		return (a:b)
	<?> "name"


xmlName :: Parser Name
xmlName = do
	firstPart <- xmlNameComponent
	secondPart <- option Nothing $ do
		string ":"
		result <- xmlNameComponent
		return $ Just result
	return $ case secondPart of
		Nothing -> Name "" firstPart
		Just secondPart -> Name firstPart secondPart

xmlComment :: Parser String
xmlComment = do
	string "<!--"
	result <- many ((noneOf "-") <|> (try (do
		a <- char '-'
		notFollowedBy $ char '-'
		return a)))
	string "-->"
	return result

xmlCharData :: Parser String
xmlCharData = do
	results <- many1 $ do
		many1 $ do
			notFollowedBy' $ string "]]>"
			noneOf "<&"
		<|> xmlEntity
		<|> try xmlCharDataSection
		<|> try (xmlComment >> return "")
	return $ foldl (++) "" results

xmlCharDataSection :: Parser String
xmlCharDataSection = do
	string "<![CDATA["
	result <- many1 $ do
		notFollowedBy' $ string "]]>"
		anyChar
	string "]]>"
	return result

xmlCharDataExcluding :: String -> Parser String
xmlCharDataExcluding excluded = do
	results <- many $ do
		many1 $ do
			noneOf excluded
		<|> xmlEntity
	return $ foldl (++) "" results


-- See http://www.haskell.org/pipermail/haskell-cafe/2005-November/012236.html
-- and http://www.haskell.org/pipermail/haskell/2004-February/013623.html
notFollowedBy' :: Show a => GenParser tok st a -> GenParser tok st ()
--notFollowedBy' p = do { a <- try p ; unexpected (show a) } <|> return ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())

xmlEntity :: Parser String
xmlEntity =
	try (do string "&amp;" ; return "&") <|> try (do string "&quot;" ; return "\"")
		<|> try (do string "&lt;" ; return "<") <|> try (do string "&gt;" ; return ">")
		<|> try (do string "&apos;" ; return "'") <|> try xmlCharRef

xmlCharRef :: Parser String
xmlCharRef = do
	string "&#"
	value <- decimal <|> (string "x" >> hexadecimal)
	result <- return [chr value]
	string ";"
	return result

parseReadS :: (Parser String) -> (ReadS a) -> Parser a
parseReadS parser readS = do
	string <- parser
	((value, _) : _) <- return $ readS string
	return value

decimal :: (Num a) => Parser a
decimal = parseReadS (many1 digit) readDec

hexadecimal :: (Num a) => Parser a
hexadecimal = parseReadS (many1 hexDigit) readHex

xmlSpace :: Parser ()
xmlSpace = do
	many1 $ oneOf " \t\r\n"
	return ()

xmlEq :: Parser ()
xmlEq = do
	option () xmlSpace
	string "="
	option () xmlSpace
	return ()

xmlStartTag :: Parser Tag
xmlStartTag = do
	string "<"
	name <- xmlName
	attributes <- xmlAttributes
	option () xmlSpace
	string ">"
	return $ Tag name attributes []

xmlEndTag :: Parser Name
xmlEndTag = do
	string "</"
	name <- xmlName
	option () xmlSpace
	string ">"
	return name

xmlEmptyTag :: Parser Tag
xmlEmptyTag = do
	string "<"
	name <- xmlName
	attributes <- xmlAttributes
	option () xmlSpace
	string "/>"
	return $ Tag name attributes []

xmlBalancedTag :: Parser Tag
xmlBalancedTag = do
	try xmlEmptyTag
	<|> do
		Tag name attributes _ <- xmlStartTag
		content <- many $ try xmlContent
		endName <- xmlEndTag
		if name /= endName
			then fail $ "Closing tag " ++ (show endName)
				++ " does not match opening tag " ++ (show name) ++ "."
			else return ()
		return $ Tag name attributes content

xmlAttributes :: Parser [Attribute]
xmlAttributes = many (try $ xmlSpace >> xmlAttribute)

xmlAttribute :: Parser Attribute
xmlAttribute = do
	name <- xmlName
	xmlEq
	value <- xmlAttributeValueDoubleQuotes <|> xmlAttributeValueSingleQuotes
	return $ Attribute name value

xmlAttributeValueDoubleQuotes :: Parser String
xmlAttributeValueDoubleQuotes = do
	string "\""
	value <- xmlCharDataExcluding "<&\""
	string "\""
	return value

xmlAttributeValueSingleQuotes :: Parser String
xmlAttributeValueSingleQuotes = do
	string "'"
	value <- xmlCharDataExcluding "<&'"
	string "'"
	return value

xmlContent :: Parser Content
xmlContent = choice [
	do
		text <- xmlCharData
		return $ Text text,
	do
		subtag <- xmlBalancedTag
		return $ Subtag subtag
	]

xmlDeclaration :: Parser XMLDeclaration
xmlDeclaration = do
	string "<?xml"
	attributes <- xmlAttributes
	option () xmlSpace
	string "?>"
	return $ XMLDeclaration attributes

{-
instance Show Message where
	show (SysUnExpect string) = "SysUnExpect(" ++ string ++ ")"
	show (UnExpect string) = "SysUnExpect(" ++ string ++ ")"
	show (Expect string) = "SysUnExpect(" ++ string ++ ")"
	show (Message string) = "SysUnExpect(" ++ string ++ ")"
	showList (a:rest) = (++) $ " " ++ (show a) ++ (show rest)
	showList [] = (++) " "-}

takeParse :: Parser a -> String -> (Maybe a, String)
takeParse parser input = 
	case parse takeParse' "" input of
		Left _ -> (Nothing, input)
--		Left err -> (Nothing, show $ errorMessages err)
		Right result -> result
	where takeParse' = do
		result <- parser
		left <- getInput
		return (Just result, left)

hasChildTag :: Tag -> Name -> Bool
hasChildTag tag name = isJust $ getChildTag tag name

getChildTag :: Tag -> Name -> Maybe Tag
getChildTag (Tag _ _ content) name =
	getChildTag' content
	where
		getChildTag' [] = Nothing
		getChildTag' (Subtag tag : rest) | isNamed tag name = Just tag
		getChildTag' (_ : rest) = getChildTag' rest

getAllChildTags :: Tag -> Name -> [Tag]
getAllChildTags (Tag _ _ content) name =
	getAllChildTags' content
	where
		getAllChildTags' [] = []
		getAllChildTags' (Subtag tag : rest) | isNamed tag name
			= tag : getAllChildTags' rest
		getAllChildTags' (_ : rest) = getAllChildTags' rest

isNamed :: Tag -> Name -> Bool
isNamed tag@(Tag tagName _ _) name@(Name namespace proper)
    = tagName == name
      || (nameIsUnqualified tagName
          && getNamespace tag == namespace
          && getProperName tag == proper)

nameIsUnqualified :: Name -> Bool
nameIsUnqualified name =
	let Name namespace _ = name in namespace == ""

getProperName :: Tag -> String
getProperName tag = proper where Tag (Name _ proper) _ _ = tag

getNamespace :: Tag -> String
getNamespace tag =
	let Tag name _ _ = tag
	in if nameIsUnqualified name
		then case getAttribute tag (Name "" "xmlns") of
			Nothing -> ""
			Just value -> value
		else ""

hasAttribute :: Tag -> Name -> Bool
hasAttribute tag name = isJust $ getAttribute tag name

getAttribute :: Tag -> Name -> Maybe String
getAttribute tag name =
	getAttribute' attributes
	where
		Tag _ attributes _ = tag
		getAttribute' [] = Nothing
		getAttribute' (Attribute attName attValue : rest) =
			if attName == name then Just attValue else getAttribute' rest

getText :: Tag -> String
getText (Tag _ _ contents) =
	getText' contents
	where
		getText' [] = ""
		getText' (Text string : rest) = string ++ (getText' rest)
		getText' (_ : rest) = getText' rest
