{- |
   Module: Parse the ZYX code
  author: Peiling Yi
  [There are four function : ws,lexeme, <||>,syntacticSugar are learnt from  were learnt from [Alfredo Di Napoli,https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller/episode-5-a-simple-dsl]]
-}

module Main  where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Data.Either
import System.IO
import System.Environment (getArgs)
import Data.List
import Data.Char
import System.Directory


{- |
Expr: this datatype is defined for five kinds of expression
  1.ExprObj: hardware component assignment
  2.ExprVar:Variable assignment
  3.Exprbool:Boolent expression
  4.Exprstring:for switch and case expression
  5.ExprWait:for wait expression
-}
data Expr = Exprvar
  { name :: String
  , operation::String
  , value :: String
  }
  |Exprobj
    { name :: String
    , operation::String
    , value :: String
    }
  | Exprbool Bool
  | Exprstring String
  | Exprwait String deriving Show


{- |
--Ifcondition datatype is defined for the condition of If statement,
--which include two value constructors, IfE or IfO,IfE is Expr datatype ,
IfO is String datatype.As a result, If statement can support (x>y) or (True)
syntax
-}
data Ifcondition = IfE Expr|IfO String deriving Show

{- |
--If Datatype is defined for If statement,which includes two fields,one is the
array of Ifcondition ,another is the array of Expr.As a result , If statment
supports two types of syntax, If(x>y){expression} and If(True){expression}
-}
data If = If [Ifcondition] [Expr] deriving Show

{- |
While datatype have two fileds, one is datatype Expr and another is
the array of program.As a result , While statement supports nest syntax.
For example, While(..){While(..){...}}. or While(..)
{If(..){..}}
-}
data While=While Expr [Program] deriving Show

{- |
Statment datatype is defined for three types of statement, which has three
value constructors: If,Switch and While.
-}
data Statement = I If|S Switch|W While deriving Show

{- |
Switch datatype is defined for Switch statement, which has two fields, one is
String and another is the array of Case.As a result, Switch support the syntax
like: Switch (state){Case 1:.Break
                    Case 2:..Break
                    Case ..:..}
-}
data Switch = Switch String [Case] deriving Show
data Case  = Case String [Expr] deriving Show

{- |
Program datatype is defined for all program.There are two value constructors
Expr and Statement. As a result, every line of program ,except bracket, belongs to Expr or
Statement.
-}
data Program= E Expr|M Statement deriving Show

{- |
  Parse "True" be True
--sample:
input:parse boolTrue "" "True"
output: right "True"
-}
boolTrue :: Parser Bool
boolTrue = (string "True") *> pure True

{- |
  Parse "False" be False
--sample:
input:parse boolFalse "" "False"
output:right "False"
-}
boolFalse :: Parser Bool
boolFalse = (string "False") *> pure False

{- |The function bool is for combining parser "True" or "False" to Boolent True or False
Sample:
input:parse bool "" "False"
output:right "False"
-}

bool :: Parser Bool
bool = boolTrue <|>boolFalse

{- |
--The function wait is for paresing "Wait" expression, which is calling Thread::wait() in the
millisecond range
Sample:
input:parse wait "" "Wait=0.2"
output:Right "0.2"
-}
wait:: Parser String
wait = do
  lexeme (syntacticSugar "\n")
  lexeme (syntacticSugar "\t")
  lexeme(string "Wait")
  lexeme(string "=")
  n<- lexeme(many(oneOf "0123456789."))
  lexeme (syntacticSugar "\n")
  lexeme (syntacticSugar "\t")
  return n

{- | These two functions,ws and lexeme,
,which is used for skiping " " in parsing line
example:parse ws "" "   fsdfs   "
right "  "
-}

ws :: Parser String
ws = many (oneOf " ")

--input: parse (lexeme stringLike) "" "  abc  "
--output: Right "abc  "

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws


stringLike :: Parser  String
stringLike = many (oneOf "abcdefghijklmnopqrstuvwxyz0.123456789CDXYZS !+-")



{- | The function (<||>) which basically takes two parsers and try to match the first one on the target;
it it fails it tries the second one.
--}
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

{- |
The function syntacticSugar is used to ensure that  even the string we expected is
not exist,The program still can carry on parsing, so it is signification to secure
the safe of parsing.
Sample:
input: parse ((syntacticSugar "sfsd")) ""  "sfsd",
output:Right (Just "sfsd")
input parse ((syntacticSugar "sfsd")) ""  "fsd",
output:Right Nothing
--}

syntacticSugar :: String -> Parser (Maybe String)
syntacticSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing
{- |
parse the DSL object to c++ proper object name
Sample:
input:parse objectName ""  "Redled",
output:Right "LED1"
--}
objectName :: Parser String
objectName = (string "Redled" *> (pure  "LED1"))
       <||> (string "Greenled" *> (pure  "LED2"))
       <||> (string "Blueled" *> (pure  "LED3"))
       <||> (string "Touchpad" *>(pure  "TSISensor"))
       <||> (string "Acc" *>(pure  "Acc"))

{- |
parse the DSL reserve word to c++ object value
sample:
input:parse matchValue ""  "On"
output:Right "0"
--}
matchValue::Parser String
matchValue= (string "On" *>(pure "0"))
        <||> (string "Off" *>(pure "1"))
        <||> (string "Red" *>(pure  "1"))
        <||> (string "Green" *>(pure "2"))
        <||> (string "Blue" *>(pure "3"))
        <||> (string "Switch" *>(pure "Switch"))


--parse the DSL operation to c++ operation
matchOpt::Parser String
matchOpt = (string "=" *> (pure "="))
              <||> (string "And" *> (pure "&&"))
              <||> (string "Or" *> (pure "||"))
              <||> (string ">" *> (pure ">"))
              <||> (string "<" *> (pure "<"))
              <||> (string "Eq" *> (pure "=="))

{- |
--parse the DSL object assignment to self-datatype Exprobj
--Sample:
Input:parse exprObject "" "green=Greenled"
Output:Right (Exprobj {name = "green", operation = "=", value = "LED2"})
--}
exprObject::Parser Expr
exprObject = do
        lexeme (syntacticSugar "\n")--contorl format
        lexeme (syntacticSugar "\t")
        s <- lexeme stringLike
        ms <- lexeme matchOpt
        name <- lexeme objectName
        lexeme (syntacticSugar "\n")
        lexeme (syntacticSugar "\t")
        --string "\n"
        return $ Exprobj s ms name

--parse the DSL variable assignment to self-datatype Exprvar
exprValue::Parser Expr
exprValue = do
        lexeme (syntacticSugar "\n")
        lexeme (syntacticSugar "\t")
        name <- lexeme stringLike
        op <- lexeme matchOpt
        value <- (lexeme matchValue)<|>(lexeme stringLike)
        --string "\n"
        lexeme (syntacticSugar "\n")
        lexeme (syntacticSugar "\t")
        return $ Exprvar name op value

--parse the DSL "True" or "False" to self-datatype Exprbool
exprBool::Parser Expr
exprBool = do
        re<-lexeme (Exprbool <$> bool)
        return re

--parse the DSL "Wait" to self-datatype Exprwait
exprWait::Parser Expr
exprWait = do
        re<-lexeme (Exprwait <$> wait)
        lexeme (syntacticSugar "\n")
        lexeme (syntacticSugar "\t")
        return re

--parse the DSL expression to be one value constructor of Expr
exprP::Parser Expr
exprP= exprWait <||> exprBool <||> exprObject<||> exprValue

{-
parse a block DSL expression to be the array of Expr
for example
input:parse exprarray "" "led=Greenled\nWait=4\nled=Redled"
output:Right [Exprobj {name = "led", operation = "=", value = "LED2"},Exprobj {name = "led", operation = "=", value = "LED1"}]
-}
exprArray::Parser [Expr]
exprArray= many1 exprP

{-
parse "if (condtion)" to be one value constructor of Ifcondtion datatype
-}
ifconP::Parser Ifcondition
ifconP = (IfE <$>(lexeme exprP)) <||> (IfO<$>(lexeme matchOpt))

{-
parse "If" statement to be self-define datatype If
for example
input:parse ifP "" "If(x > y\n) and (x<y\n) {x=y\n x=z\n}"
output:Right (If (Exprvar {name = "x", operation = ">", value = "y"}) [Exprvar {name = "x", operation = "=", value = "y"},Exprvar {name = "x", operation = "=", value = "z"}])
-}
ifP::Parser If
ifP = do
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    --lexeme (syntacticSugar "If")
    lexeme (string "If")
    lexeme (syntacticSugar "(")
    s<- many1 ifconP
    lexeme (syntacticSugar ")")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "{")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    ts<-many1 exprP
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "}")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    return $ If s ts

{-
caseP parse statment "Case" to be self-define datatype "Case"
for example:
input:parse caseP "" "Case x:  x=y\n x=z\n"
output:Right (Case "x" [Exprvar {name = "x", operation = "=", value = "y"},Exprvar {name = "x", operation = "=", value = "z"}])
lefe is error if there is mistake
-}
caseP::Parser Case
caseP = do
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    --lexeme (syntacticSugar "Case ")
    lexeme(string "Case")
    s <- (lexeme matchValue)<|>(lexeme stringLike)
    lexeme (syntacticSugar ":")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    ms<-many1 exprP
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "Break")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    return $ Case s ms

{-
The function switchP parse statment "Switch" to be self-define datatype "Switch"
for example:
input:parse caseP "" "Case x:  x=y\n x=z\n"
output:Right (Case "x" [Exprvar {name = "x", operation = "=", value = "y"},Exprvar {name = "x", operation = "=", value = "z"}])
lefe is error if there is mistake
-}
switchP::Parser Switch
switchP = do
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    --lexeme (syntacticSugar "Switch")
    lexeme (string "Switch")
    lexeme (syntacticSugar "(")
    s<-lexeme stringLike
    lexeme (syntacticSugar ")")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "{")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    ms<-many1 caseP
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "}")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    return $ Switch s ms

{-
The function whileP parse statment "While" to be self-defined datatype "While"
for example:
input:parse whileP "" "While(True)\n{\n a=b\nc=d\n}"
output:Right (While (Exprbool True) [E (Exprvar {name = "a", operation = "=", value = "b"}),E (Exprvar {name = "c", operation = "=", value = "d"})])
-}
whileP::Parser While
whileP = do

    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme(string "While")
    --lexeme (syntacticSugar "While")
    lexeme (syntacticSugar "(")
    --string "While"
    s<-lexeme exprP
    lexeme (syntacticSugar ")")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "{")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    ms<-many1 (lexeme program)
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    lexeme (syntacticSugar "}")
    lexeme (syntacticSugar "\n")
    lexeme (syntacticSugar "\t")
    return $ While s ms


--The function statementP parse statement  be one value constructor of self-defined datatype "Statement"
statementP::Parser Statement
statementP = lexeme(I<$>ifP)<||>lexeme(S<$>switchP)<||>lexeme(W<$>whileP)

--The function program parse one line of program be one value constructor of self-defined datatype "Program"
program::Parser Program
program = (E<$>exprP)<||>(M<$>statementP)

--The function programP parse the program be the array of Program
programP::Parser [Program]
programP= many1 program

--Expr datatype checking
isExprBool:: Expr->Bool
isExprBool(Exprbool _)=True
isExprBool _=False

--Expr datatype checking
isExprString:: Expr->Bool
isExprString(Exprstring _)=True
isExprString _=False

--Expr datatype checking
isExprWait:: Expr->Bool
isExprWait(Exprwait _)=True
isExprWait _=False

--Expr datatype checking
isExprVar::Expr->Bool
isExprVar(Exprvar _ _ _)=True
isExprVar _ =False

--Expr datatype checking
isExprObj::Expr->Bool
isExprObj(Exprobj _ _ _)=True
isExprObj _ =False

--extracting the value from Expr datatype data
getobjectname::Expr->String
getobjectname (Exprobj name _ _) = name

--extracting the value from Expr datatype data
getvarname::Expr->String
getvarname (Exprvar name _ _) = name

--extracting the value from Expr datatype data
getobjectoperation::Expr->String
getobjectoperation (Exprobj _ op _) = op

--extracting the value from Expr datatype data
getvaroperation::Expr->String
getvaroperation (Exprvar _ op _) = op

--extracting the value from Expr datatype data
getobjectvalue::Expr->String
getobjectvalue (Exprobj _ _ value) = value

--extracting the value from Expr datatype data
getvarvalue::Expr->String
getvarvalue (Exprvar _ _ value) = value

--extracting the value from Expr datatype data
getbool::Expr->String
getbool (Exprbool value) = if(value) then "true" else "false"

--extracting the value from Expr datatype data
getstring::Expr->String
getstring (Exprstring value) = value

--extracting the value from Expr datatype data
getwait::Expr->String
getwait (Exprwait value) = value

--replacing  the definition of component of board from ZYX code to C++ code.
convertExprObj::Expr->String
convertExprObj x
      |(value=="LED1")||(value=="LED2")||(value=="LED3")="PwmOut "++name++"("++value++");"
      |(value=="Acc")="MMA8451Q  "++name++"(PTE25, PTE24, MMA8451_I2C_ADDRESS);"
      |(value=="TSISensor")="TSISensor  "++name++";"
      |otherwise=value
    where value=(getobjectvalue x)
          name = (getobjectname x)

--replace the function of ZYX to the function of Mbed SDK  and add datatype of C++
convertExprValue::String->String->String->String
convertExprValue x y z
      |(filter(=='X')z=="X")=x++y++"1.0-abs("++(filter (/='X') z)++"getAccX()"++")"
      |(filter(=='Y')z=="Y")=x++y++"1.0-abs("++(filter (/='Y') z)++"getAccY()"++")"
      |(filter(=='Z')z=="Z")=x++y++"1.0-abs("++(filter (/='Z') z)++"getAccZ()"++")"
      |(y=="Switch")=x++y++"!"++x
      |(filter(=='D')z=="D")="int "++x++y++(filter (/='D') z)++"readDistance()"
      |otherwise=x++y++z
--  |(filter(==False)(map isDigit z)==[])=if (read z>2) then "int "++x++y++z else x++y++z
-- |(x=="state")&&(x/="int state")="int "++"state"++y++z
--Expr datatype be C++ code string
convertExpr::Expr->String
convertExpr x
      |(isExprObj x)= convertExprObj x
      |(isExprVar x)=(convertExprValue(getvarname x)(getvaroperation x)(getvarvalue x))++";"
      |(isExprBool x)=(getbool x)
      |(isExprString x)=(getstring x)++";"
      |(isExprWait x)="wait("++(getwait x)++")"++";"

--extracting the If value from Statement datatype data
getIf::Statement->If
getIf(I value)=value

--extracting the Switch value from Statement datatype data
getSwitch::Statement->Switch
getSwitch(S value)=value

--extracting the While value from Statement datatype data
getWhile::Statement->While
getWhile(W value)=value

--Statement datatype checking
isIf::Statement->Bool
isIf(I _)=True
isIf _=False

--Statement datatype checking
isSwitch::Statement->Bool
isSwitch(S _)=True
isSwitch _=False

--Statement datatype checking
isWhile::Statement->Bool
isWhile(W _)=True
isWhile _=False

--Ifcondition datatype checking
isIfE::Ifcondition->Bool
isIfE(IfE _ )=True
isIfE _=False

--Ifcondition datatype checking
isIfO::Ifcondition->Bool
isIfO(IfO _ )=True
isIfO _=False

--extracting the Ifcondition value from Expr datatype data
getIfE::Ifcondition->Expr
getIfE(IfE value)=value

--extracting the Ifcondition value from String datatype data
getIfO::Ifcondition->String
getIfO(IfO value)=value

--convert If statement code of ZYX into C++ code
convertIfcondition::Ifcondition->String
convertIfcondition x
        |(isIfE x)=(convertExpr (getIfE x))
        |(isIfO x)=getIfO x

--extracting condition value from If datatype
getIfcondition::If->String
getIfcondition(If value _ )=filter (/=';')("if( "++(intercalate " " (map convertIfcondition value))++")")

--extracting string array from If datatype
getIfexpr::If->[String]
getIfexpr(If _ value )=(map convertExpr value)

--convert the If datatype into String array
convertIf::If->[String]
convertIf x =(getIfcondition x):["{"]++(getIfexpr x)++["}"]

--extract the While condition value
getWhilecondition:: While->String
getWhilecondition(While value _) = convertExpr value

--extract the While program value
getWhilprogram:: While->[String]
getWhilprogram(While _ value) = convertPrograms value

--convert the While condition datatype into String
convertWhile:: While->[String]
convertWhile x=("while("++filter (/=';') (getWhilecondition x)++")"++"{"):(getWhilprogram x) ++["}"]


--extract the Swich state value
getSwitchstate::Switch->String
getSwitchstate(Switch value _)="switch("++"state"++")"

--extract the Swich case value
getSwitchcase::Switch->[[String]]
getSwitchcase(Switch _ value)=map (getCase) value


--extract the Swich case state value
getCasestate:: Case->String
getCasestate(Case value _)="case "++value++":"

--extract the Swich case program value
getCaseexpr:: Case->[String]
getCaseexpr(Case _ value)=map convertExpr value

--extract all case value
getCase:: Case->[String]
getCase x= getCasestate(x): getCaseexpr(x)++["break;"]

--convert Switch data type be string array
convertSwitch::Switch->[String]
convertSwitch x=(getSwitchstate x):"{":(map unlines (getSwitchcase x))++["}"]

--convert Statement data type be string array
convertStatement::Statement->[String]
convertStatement x
      |(isIf x)= convertIf (getIf x)
      |(isSwitch x)= convertSwitch (getSwitch x)
      |(isWhile x)= convertWhile (getWhile x)

--extract the Expr value from program
getE::Program->Expr
getE(E ex)=ex

--extract the Statement value from program
getM::Program->Statement
getM(M st)=st

--program data type checking
isE:: Program->Bool
isE(E _)=True
isE _ =False

--program data type checking
isM:: Program->Bool
isM(M _)=True
isM _ =False

--convert one line of ZYX code to C++ code
convertProgram::Program->[String]
convertProgram x
      |(isE x)=[convertExpr (getE x)]
      |(isM x)=convertStatement (getM x)
--convert the whole ZYX codes to C++ codes
convertPrograms::[Program]->[String]
convertPrograms x= map unlines (map convertProgram x)

--adding one of  header file of C++ according to Object assignment
includef::String->String
includef x
      |((take 6 x)=="PwmOut") = "#include \"mbed.h\"\n"
      |((take 8 x)=="MMA8451Q") = "#include \"MMA8451Q.h\"\n#define MMA8451_I2C_ADDRESS (0x1d<<1)\n"
      |((take 9 x)=="TSISensor") ="#include \"TSISensor.h\"\n"
      |otherwise=""

--adding a group of header file of C++
includefs::[String]->[String]
includefs[]=[]
includefs(x:xs)=(includef x):includefs xs

--removing the duplicated header file
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

--The function “mainconvert” assemble all function together and control the general flow to write C++/C file.
mainconvert :: [String]->IO ()
mainconvert [inputname,outputname]=do
  text<-readFile inputname
  let res1 = lines(text)
  let res2 = unlines(res1)
  let res3 = rights [parse programP "" res2]
  if(null(res3))
    then print res2
    else do
      let res4= head(res3)
      let res5=(convertPrograms res4)
      let res6 =(uniq(includefs res5))++["int main() {\n"]++res5++["\n}"]
      writeFile outputname (unwords res6)
      fileExist <- doesFileExist outputname
      case fileExist of
               True -> putStrLn "coding is successful"
               False -> putStrLn "Sorry,coding was not successful "
  --    print res3
  --    print res2

--get the parameters for input filename and output filename
main = do
    args <- getArgs
    if((length args)==2) then mainconvert args else mainconvert ["mbedin.txt","mbedout.c"]
