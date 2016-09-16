% Resources
% ---------
%
% http://www.math-linux.com/latex-26/article/how-to-make-a-presentation-with-latex-introduction-to-beamer

\documentclass{beamer}

\usepackage{verbatim}
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}
\usepackage[latin1]{inputenc}

\usetheme{Warsaw}
\title[HaL 2016]{Simple blog engine with shape functors and generic eliminators for ADTs}
\author{Andor Penzes}
\institute{zerobuzz}
\date{September 15, 2016}
\begin{document}

% Presentation starts here
\begin{frame}
\titlepage
\end{frame}

% Hidden frames for module definition and import
\begin{frame}<presentation:0>[fragile]
\begin{code}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
module HAL2016 where

import Prelude hiding (div, length)
import Data.Function (on)
import Data.List (sortBy)
import Data.String
import Data.Time (UTCTime)

import System.Directory
import System.FilePath
import Text.Blaze.Html5 hiding (menu)
import Text.Blaze.Html5.Attributes as A hiding (id, title)
import Text.Pandoc.Definition (Pandoc(..), Block(..), Inline(..))
import Text.Pandoc.Options
import Text.Pandoc.Writers.HTML
import Text.Blaze.Html.Renderer.String

import GHC.TypeLits

\end{code}
\end{frame}



\begin{frame}[fragile]{Introduction}
An experiment on a blog engine is complicated
enough, to be a real world like problem. It is small enough
to code it in a few hours after work.
\end{frame}



\begin{frame}[fragile]{Eliminators}
This will be confusing as the same phenomenon has many names
in the literature.
\begin{itemize}
\item Generic eliminator
\item Eliminator
\item Catamorhism
\item Initial algebra
\item Template function
\end{itemize}
The full power of the generic eliminators are connected
to dependent typed programming.
\\~\\
Haskell is not dependent yet, let's use a simple approach.
\end{frame}



\begin{frame}[fragile]
In theory, there is no difference between theory and practice. But, in practice, there is. (Jan L. A. van de Snepscheut)
\pause\\~\\
This presentation is about the definitions and the practical use of generic
eliminators.
\end{frame}



\begin{frame}[fragile]
\begin{footnotesize}
\begin{verbatim}
-- Abstract deepsense. (Matthias Fishmann)
module Eliminators.Theory where
\end{verbatim}
\end{footnotesize}
\end{frame}



\begin{frame}[fragile]{Basic example}
\begin{code}
data List a
  = Empty
  | Cons a (List a)

length :: List a -> Int
length Empty       = 0
length (Cons _ xs) = 1 + length xs
\end{code}
\end{frame}



\begin{frame}[fragile]{Abstract the recursion}
For every ADT we can define an algebra based on its structure.
\pause\\~\\
Eliminator for an ADT captures the structure structure of the ADT,
when the ADT is recursive the eliminator is applied for the recursion.
\pause
\begin{code}
list_elim :: (b, a -> b -> b) -> List a -> b
list_elim (empty, cons) Empty       = empty
list_elim (empty, cons) (Cons a as) =
    cons a (list_elim (empty, cons) as)
\end{code}
\pause
\begin{code}
length_alg :: (Int, a -> Int -> Int)
length_alg = (0, \_ n -> 1 + n)

length' :: List a -> Int
length' = list_elim length_alg
\end{code}
\end{frame}



\begin{frame}[fragile]{Abstract the list shape}
Shape functors.
\\~\\
Eliminators for an ADT can be separated into the shape of the
ADT and the recursion scheme.
\begin{code}
data ListShape a rec
  = EmptyS
  | ConsS a rec
  deriving (Show)

newtype Fix f = In { unFix :: f (Fix f) }

type ListF a = Fix (ListShape a)

e   = In EmptyS
ae  = In (ConsS 1 e)
aae = In (ConsS 2 ae)
\end{code}
\end{frame}



\begin{frame}[fragile]{Abstract the list shape}
\begin{code}
listElim :: (b, a -> b -> b) -> ListF a -> b
listElim (empty, cons) (In EmptyS)      = empty
listElim (empty, cons) (In (ConsS a s)) =
    cons a (listElim (empty, cons) s)

length'' = listElim (0, (\_ n -> (1 + n)))
\end{code}
\end{frame}



\begin{frame}[fragile]{Abstract the cases of list shape}
Let's rename listElim to listCata as we abstracting
away from the value processing.
\begin{code}
listCata :: (ListShape a s -> s) -> ListF a -> s
listCata alg (In EmptyS)      = alg EmptyS
listCata alg (In (ConsS a s)) = alg (ConsS a (listCata alg s))

lengthAlg :: ListShape a Int -> Int
lengthAlg EmptyS      = 0
lengthAlg (ConsS _ n) = 1 + n

length''' = listCata lengthAlg
\end{code}
\end{frame}



\begin{frame}[fragile]{Shape functor}
\begin{code}
instance Functor (ListShape a) where
    fmap f EmptyS      = EmptyS
    fmap f (ConsS a s) = ConsS a (f s)
\end{code}
\end{frame}



% TODO: Definition of initial algebra.
\begin{frame}[fragile]{Catamorphism}
Let's abstract the shape functor.
In Category theory the algebras are defined for
functors. Many algebra
can be defined for the given functor.
\begin{code}
-- newtype Fix f = In { unFix :: f (Fix f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f s -> Fix f -> s
cata alg = alg             -- Compute the result from the partials
         . fmap (cata alg) -- Compute the partial results
         . unFix           -- Step inside

length'''' = cata lengthAlg
\end{code}
Catamorphism is a recursion scheme.
\end{frame}



\begin{frame}[fragile]{More than cata}
Factorial?
\\~\\
Catamorhisms are not powerfull enough, there is a zoo
of morphisms. We need an another type of morphism to
be able to define the factorial function.
\\~\\
http://hackage.haskell.org/package/fixplate
\end{frame}



\begin{frame}[fragile]
\begin{footnotesize}
\begin{verbatim}
-- Concrete nonsense.
module Eliminators.Practice where
\end{verbatim}
\end{footnotesize}
\end{frame}



\begin{frame}[fragile]{Real World Development}
Find the balance between the abstractions and concreteness.
\pause\\~\\
Real world development usually
\\~\\
\begin{itemize}
\item is not too abstract
\item uses modular approach
\item is powerful enough to cover the problems.
\end{itemize}
\pause
Use Fix, Shape functors and cata if your data types are
tend to be recursive and there is a high probability
of changes
\\~\\
Use Eliminators otherwise.
\end{frame}



\begin{frame}[fragile]{The godfather of all eliminators}
First and well known lazy generic eliminator in every programming language!
\begin{code}
boolElim t f e = if e then t else f
\end{code}
or
\begin{code}
boolElim' t f e = case e of
    True  -> t
    False -> f
\end{code}
\end{frame}



\begin{frame}[fragile]{More eliminators}
With Haskell we can create eliminators for every ADT, based on the structure
of the ADT. With laziness generic eliminators can serve as template
functions for the values we work with.
\begin{code}
maybeElim n j m = case m of
    Nothing -> n
    Just x  -> j x
\end{code}
\begin{code}
eitherElim l r e = case e of
    Left x  -> l x
    Right y -> r y
\end{code}
\end{frame}



\begin{frame}[fragile]{Composition}
Composition of eliminators comes from the structural
induction on the shape of ADT.
\begin{code}
compExample =
    eitherElim
      (eitherElim
        (show . (1+))
        ("x=" ++))
      (maybeElim "NaN" (show . floor))
\end{code}
Using intendation helps a lot. It is very similar to the pointfree style.
\end{frame}



\begin{frame}[fragile]{Design recipe}
\begin{itemize}
\pause \item Create an ADT
\pause \item Create its eliminator based on the stucture
\pause \item Encapsulate this definitions in a module
\pause \item Sometimes it is useful to add a typed hole, which can carry out information
\pause \item Create algebras to define functions with eliminators
\end{itemize}
\end{frame}


\begin{frame}[fragile]{Real world data}
In real world examples the information usualy organized in a
tree shaped data.
\end{frame}


% Examples from the blog
\begin{frame}[fragile]{Entry}
\begin{code}
data Entry a = Entry {
    e_hole  :: a
  , e_lines :: Pandoc
  } deriving (Functor, Eq, Show)

type EntryAlgebra a b = (a -> Pandoc -> b)

entryElim :: EntryAlgebra a b -> Entry a -> b
entryElim alg (Entry hole lines) = alg hole lines
\end{code}
Type hole in Entry. With a type hole we can expres more computational
power and can convert our regular data type to a shape functor, and
if we need we can use it in Fix compuations.
\end{frame}



\begin{frame}[fragile]{TopicName}
\begin{code}
data TopicName a = TopicName {
    tn_hole :: a
  , tn_name :: Pandoc
  } deriving (Functor, Eq, Show)

type TopicNameAlgebra a b = (a -> Pandoc -> b)

topicNameElim :: TopicNameAlgebra a b -> TopicName a -> b
topicNameElim alg (TopicName hole name) = alg hole name
\end{code}
\end{frame}



\begin{frame}[fragile]{Topic}
\begin{code}
data Topic a = Topic {
    t_hole      :: a
  , t_topicName :: TopicName a
  , t_entries   :: [Entry a]
  } deriving (Functor, Eq, Show)
\end{code}
\pause
How to define an eliminator and algebras for Topic?
\begin{code}
type TopicAlgebra a t e es p =
    ( TopicNameAlgebra a t
    , EntryAlgebra a e
    , ListAlgebra e es
    , a -> t -> es -> p)
topicElim :: TopicAlgebra a t e es p -> Topic a -> p
topicElim (topicNameAlg, entryAlg, entriesAlg, combine)
          (Topic hole topicName entries)
  = combine
        hole
        (topicNameElim topicNameAlg topicName)
        (listElim_ entriesAlg (entryElim entryAlg <$> entries))
\end{code}
\end{frame}



\begin{frame}[fragile]{Blog}
\begin{code}
data Blog a = Blog {
    b_hole    :: a
  , b_summary :: Pandoc
  , b_topics  :: [Topic a]
  } deriving (Functor, Eq, Show)

type BlogAlgebra a t e es p bs b =
    ( TopicAlgebra a t e es p
    , ListAlgebra p bs
    , a -> Pandoc -> bs -> b )

blogElim :: BlogAlgebra a t e es p bs b -> Blog a -> b
blogElim (topic, topicList, combine)
         (Blog hole summary topics)
  = combine
        hole
        summary
        (listElim_ topicList (topicElim topic <$> topics))
\end{code}
\end{frame}



\begin{frame}[fragile]{Renderer}
\begin{code}
renderPages :: FilePath -> (NavPath -> Html -> Html)
            -> Blog FileProperties -> IO ()
renderPages outDir frame = blogElim render where
  render = (topic, sequence_, topicList)
  topic  = (topicName, entry, entryList, topicNameEntryList)

  entry fp pandoc = do
    writeFile (outDir </> (markdownPathToHTMLPathFP fp))
              (renderHtml . frame NavBackward $ pandoc2html pandoc)
    return (fp, firstHeader pandoc)

  entryList = (return [], \x xs -> (:) <$> x <*> xs)
  topicList _ _ ts = ts
  sequence_ = (return (), (>>)) -- Monoid instance of monads
\end{code}
\end{frame}



\begin{frame}[fragile]{Renderer}
\begin{code}
  topicName fp pandoc = do
    createDirectoryIfMissing True $ outDir </> markdownPathToHTMLDir fp
    return (\content -> writeFile
                (outDir </> (markdownPathToHTMLPathFP fp))
                (renderHtml $ frame NavInPlace content)
           , pandoc
           )

  topicNameEntryList _ topicName entryList = do
    (topicPage, pandoc) <- topicName
    headers <- entryList
    topicPage $ do -- :: Html
      pandoc2panel pandoc
      topicsList headers
\end{code}
\end{frame}


\begin{frame}[fragile]{Drawbacks / Solutions}
Drawbacks:
\begin{itemize}
\pause \item If the type is the same in every case eliminators can be easily swapped
\pause \item No names of the cunstructors are given
\end{itemize}
\pause
Solutions:
\begin{itemize}
\pause \item Create an ADT for the algebra and name the constructors
\pause \item Use the new Symbol types as type parameter to name the different cases
\end{itemize}
\end{frame}


\begin{frame}[fragile]{Named parameters}
Use named parameters
\begin{code}
data Param (n :: Symbol) a = Param a

maybeElimNamed
    :: (Param "nothing" b) -> (Param "just" (a -> b)) -> Maybe a -> b
maybeElimNamed (Param nothing) (Param just) = \case
    Nothing -> nothing
    Just x  -> just x

test = maybeElimNamed
            (Param 0    :: Param "nothing" Int)
            (Param (1+) :: Param "just" (Int -> Int))
\end{code}
\end{frame}


\begin{frame}[fragile]{Lenses}
Connection to lenses
\begin{itemize}
\item Lenses are coalgebras, composition works via function composition
\item Eliminators use algebras, composition works via tupling
\item Eliminators are like universal properties for an ADT
\end{itemize}
\end{frame}


\begin{frame}[fragile]{Future work}
More...
\begin{itemize}
\item Use template haskell to generate eliminators from the ADT
\item Use generics-sop library to generate eliminators
\item Create a library
\end{itemize}
\end{frame}



\begin{frame}[fragile]{Conclusion}
Conclusion
\begin{itemize}
\item Similar to point free style
\item Algorithms are compact, but still understandable
\item Composition done by chaining or tupling of algebras
\end{itemize}
https://github.com/andorp/andorp.github.io/tree/master/haskell
\end{frame}



% LAST frame: Technical frame to hide implementations
\begin{frame}<presentation:0>[fragile]
\begin{code}

type ListAlgebra a b = (b, a -> b -> b)

listElim_ :: ListAlgebra a b -> [a] -> b
listElim_ (nil, cons) = foldr cons nil

instance Show (f (Fix f)) => Show (Fix f) where
    showsPrec p (In f) = showsPrec p f

data FileProperties = FileProperties {
    path             :: FilePath
  , modificationTime :: UTCTime
  } deriving (Eq, Show)

data NavPath
  = NavForward String
  | NavInPlace
  | NavBackward
  deriving (Show, Eq)

markdownPathToHTMLDir :: FileProperties -> String
markdownPathToHTMLDir = dropExtension . path

markdownPathToHTMLPath :: (IsString string) => FilePath -> string
markdownPathToHTMLPath = fromString . flip addExtension "html" . dropExtension

markdownPathToHTMLPathFP :: FileProperties -> FilePath
markdownPathToHTMLPathFP = markdownPathToHTMLPath . path


topicsList :: [(FileProperties, String)] -> Html
topicsList entries = div ! class_ "list-group" $
  mapM_ link . reverse $ sortBy (compare `on` modificationTime . fst) entries
  where
    link (fp, headLine) = a ! href (markdownPathToHTMLPath $ path fp)
                            ! class_ "list-group-item" $ fromString headLine

pandoc2html :: Pandoc -> Html
pandoc2html = writeHtml cfg where
  cfg = def { writerTableOfContents = True
            , writerSectionDivs = True
            , writerColumns = 80
            , writerHtml5 = True
            , writerHighlight = True
            , writerHTMLMathMethod = MathML Nothing
            }

pandoc2panel :: Pandoc -> Html
pandoc2panel pandoc = do
  div ! class_ "panel panel-default" $ do
    div ! class_ "panel-heading" $ firstHeader pandoc
    div ! class_ "panel-body" $ pandoc2html $ removeFirstHeader pandoc

firstHeader :: (IsString string) => Pandoc -> string
firstHeader (Pandoc _meta blocks) =
  case filter isHeader blocks of
    []                     -> error "No header is found"
    (Header _ _ inlines:_) -> fromString $ inlinesToString inlines

removeFirstHeader :: Pandoc -> Pandoc
removeFirstHeader (Pandoc meta blocks) = Pandoc meta (removeFirst isHeader blocks)
  where
    removeFirst _p []     = []
    removeFirst p  (x:xs)
      | p x       = xs
      | otherwise = x : removeFirst p xs

inlinesToString :: [Inline] -> String
inlinesToString = concatMap strInlineToString

isHeader (Header _ _ _) = True
isHeader _              = False

strInlineToString :: Inline -> String
strInlineToString (Str str) = str
strInlineToString Space     = " "
strInlineToString x         = error $ "No string inline: " ++ show x

\end{code}
\end{frame}

\end{document}
