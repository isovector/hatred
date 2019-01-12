{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}

module Algebra where

import Commands
import Types
import TH

algebra = [prose|
  \Chapter{The Algebra Behind Types}

  \Section{Isomorphisms and Cardinalities}

  One of functional programming's killer features is pattern matching, as made
  possible by \Defnn{algebraic data types}{algebraic data type}. But this term
  isn't just a catchy title for things that we can pattern match on. As their name
  suggests, there is in fact an \Emph{algebra} behind algebraic data types.

  Being comfortable understanding and manipulating this algebra is a mighty
  superpower---it allows us to analyze types, find more convenient forms for them,
  and determine which operations (eg. typeclasses) are possible to implement.

  To start, we can associate each type with its \Defn{cardinality}---the number of
  inhabitants it has, ignoring bottoms. Consider the following simple type
  definitions:

  \Snip{Algebra}{Void}
  \Snip{Algebra}{Unit}
  \Snip{Algebra}{Bool}

  \Ty{Void} has zero inhabitants, and so it is assigned cardinality 0. The unit
  type \Ty{()} has one inhabitant---thus its cardinality is 1. Not to belabor
  the point, but \Ty{Bool} has cardinality 2, corresponding to its constructors
  \Hs{True} and \Hs{False}.
    |]

--   We can write these statements about cardinality more formally:

--   $$
--   \begin{align*}
--     \card{Void} &= 0 \\
--     \card{()} &= 1 \\
--     \card{Bool} &= 2 \\
--   \end{align*}
--   $$

--   Any two types that have the same cardinality will always be isomorphic to one
--   another. An \Defn{isomorphism} between types \Ty{s} and \Ty{t} is defined as a
--   pair of functions \Hs{to} and \Hs{from}:

--   \snip{Algebra}{to}

--   such that composing either after the other gets you back where you started. In
--   other words, such that:

--   $$
--   \begin{align*}
--     \Hs{to .\spaceJob{}from = id} \\
--     \Hs{from .\spaceJob{}to = id} \\
--   \end{align*}
--   $$

--   We sometimes write an isomorphism between types \Ty{s} and \Ty{t} as \iso{s}{t}.

--   If two types have the same cardinality, any one-to-one mapping between their
--   elements is exactly these \Hs{to} and \Hs{from} functions. But where does such a
--   mapping come from? Anywhere---it doesn't really matter! Just pick an arbitrary
--   ordering on each type---not necessarily corresponding to an \Ty{Ord}
--   instance---and then map the first element under one ordering to the first
--   element under the other. Rinse and repeat.

--   For example, we can define a new type that also has cardinality 2.

--   \snip{Algebra}{Spin}

--   By the argument above, we should expect \Ty{Spin} to be isomorphic to \Ty{Bool}.
--   Indeed it is:

--   \snip{Algebra}{boolToSpin1}
--   \snip{Algebra}{spinToBool1}

--   However, note that there is another isomorphism between \Ty{Spin} and
--   \Ty{Bool}:

--   \snip{Algebra}{boolToSpin2}
--   \snip{Algebra}{spinToBool2}

--   Which of the two isomorphisms should we prefer? Does it matter?

--   In general, for any two types with cardinality $n$, there
--   are $n!$ unique isomorphisms between them. As far as the math goes, any of these
--   is just as good as any other---and for most purposes, knowing that an
--   isomorphism \Emph{exists} is enough.

--   An isomorphism between types \Ty{s} and \Ty{t} is a proof that \Emph{for all
--   intents and purposes,} \Ty{s} and \Ty{t} \Emph{are the same thing.} They might
--   have different instances available, but this is more a statement about Haskell's
--   typeclass machinery than it is about the equivalence of \Ty{s} and \Ty{t}.

--   Isomorphisms are a particularly powerful concept in the algebra of types.
--   Throughout this book we shall reason via isomorphism, so it's best to get
--   comfortable with the idea now.


--   \Section{Sum, Product and Exponential Types}

--   In the language of cardinalities, \Defnn{sum types}{sum type} correspond to
--   addition. The canonical example of these is \Ty{Either a b}, which is
--   \Emph{either} an \Ty{a} or a \Ty{b}. As a result, the cardinality (remember, the
--   number of inhabitants) of \Ty{Either a b} is the cardinality of \Ty{a} plus the
--   cardinality of \Ty{b}.

--   $$
--   \card{Either a b} = \card{a} + \card{b}
--   $$

--   As you might expect, this is why such things are called \Emph{sum} types. The
--   intuition behind adding generalizes to any datatype with multiple
--   constructors---the cardinality of a type is always the sum of the cardinalities
--   of its constructors.

--   \snip{Algebra}{Deal}

--   We can analyze \Ty{Deal}'s cardinality;

--   $$
--   \begin{align*}
--     \card{Deal a b} &= \card{a} + \card{b} + \card{Bool} \\
--     &= \card{a} + \card{b} + 2
--   \end{align*}
--   $$

--   We can also look at the cardinality of \Ty{Maybe a}. Because nullary data
--   constructors are uninteresting to construct---there is only one
--   \Hs{Nothing}---the cardinality of \Ty{Maybe a} can be expressed as follows;

--   $$
--   \card{Maybe a} = 1 + \card{a}
--   $$

--   Dual to sum types are the so-called \Defnn{product types}{product type}. Again,
--   we will look at the canonical example first---the pair type \Ty{(a, b)}.
--   Analogously, the cardinality of a product type is the \Emph{product} of their
--   cardinalities.

--   $$
--   \card{(a, b)} = \card{a} \times \card{b}
--   $$

--   To give an illustration, consider mixed fractions of the form $5\frac{1}{2}$. We
--   can represent these in Haskell via a product type;

--   \snip{Algebra}{MixedFraction}

--   And perform its cardinality analysis as follows:

--   $$
--   \card{MixedFraction a} = \card{Word8} \times \card{a} \times \card{a} = 256
--   \times \card{a} \times \card{a}
--   $$

--   An interesting consequence of all of this cardinality stuff is that we find
--   ourselves able to express \Emph{mathematical truths in terms of types}. For
--   example, we can prove that $a \times 1 = a$ by showing an isomorphism between
--   \Ty{(a, ())} and \Ty{a}.

--   \snip{Algebra}{prodUnitTo}
--   \snip{Algebra}{prodUnitFrom}

--   Here, we can think of the unit type as being a monoidal identity for product
--   types---in the sense that ``sticking it in doesn't change anything.'' Because $a
--   \times 1 = a$, we can pair with as many unit types as we want.

--   Likewise, \Ty{Void} acts as a monoidal unit for sum types. To convince ourselves
--   of this, the trivial statement $a+0 = a$ can be witnessed as an isomorphism
--   between \Ty{Either a Void} and \Ty{a}.

--   \snip{Algebra}{sumUnitTo}
--   \snip{Algebra}{sumUnitFrom}

--   The function \Hs{absurd} at \ann{1} has the type \Ty{Void -> a}. It's a sort of
--   bluff saying ``if you give me a \Ty{Void} I can give you anything you
--   want.'' Despite this being a lie, because there are no \Ty{Void}s to be had in
--   the first place, we can't disprove it.

--   Function types also have an encoding as statements about cardinality---they
--   correspond to exponentialization. To give an example, there are exactly four
--   ($2^2$) inhabitants of the type \Ty{Bool -> Bool}. These functions are \Hs{id},
--   \Hs{not}, \Hs{const True} and \Hs{const False}. Try as hard as you can, but you
--   won't find any other pure functions between \Ty{Bool}s!

--   More generally, the type \Ty{a -> b} has cardinality $\card{b}^{\card{a}}$.
--   While this might be surprising at first---it always seems backwards to me---the
--   argument is straightforward. For every value of \Ty{a} in the domain, we need to
--   give back a \Ty{b}. But we can chose any value of \Ty{b} for every value of
--   \Ty{a}---resulting in the following equality.

--   $$
--   \card{a -> b} = \underbrace{\card{b} \times \card{b} \times \cdots \times
--   \card{b}}_{\card{a} \text{times}} = \card{b}^{\card{a}}
--   $$

--   \begin{exercise}
--   Determine the cardinality of \Ty{Either Bool (Bool, Maybe Bool) -> Bool}.
--   \end{exercise}
--   \begin{solution}
--   $$
--   \begin{align*}
--     &  \card{Either Bool (Bool, Maybe Bool) -> Bool} \\
--     &= \card{Bool}^{\card{Either Bool (Bool, Maybe Bool)}} \\
--     &= \card{Bool}^{\card{Bool}+\card{Bool}\times\card{Maybe Bool}} \\
--     &= \card{Bool}^{\card{Bool}+\card{Bool}\times(\card{Bool}+1)} \\
--     &= 2^{2+2\times(2+1)} \\
--     &= 2^{2+2\times 3} \\
--     &= 2^{2+6} \\
--     &= 2^{8} \\
--     &= 256
--   \end{align*}
--   $$
--   \end{solution}

--   The inquisitive reader might wonder whether subtraction, division and other
--   mathematical operations have meaning when applied to types. Indeed they do, but
--   such things are hard, if not impossible, to express in Haskell. Subtraction
--   corresponds to types with particular values removed, while division of a type
--   makes some of its values equal (in the sense of being defined equally---rather
--   than having an \Ty{Eq} instance which equates them.)

--   In fact, even the notion of differentiation in calculus has meaning in the
--   domain of types. Though we will not discuss it further, the interested reader is
--   encouraged to refer to Conor McBride's paper ``The Derivative of a Regular Type
--   is its Type of One-Hole Contexts.''\cite{one-hole}.


--   \Section{Example: Tic-Tac-Toe}

--   I said earlier that being able to manipulate the algebra behind types is a
--   mighty superpower. Let's prove it.

--   Imagine we wanted to write a game of tic-tac-toe. The standard tic-tac-toe board
--   has nine spaces, which we could naively implement like this:

--   \snip{Algebra}{TicTacToe}

--   While such a thing works, it's rather unwieldy to program against. If we wanted
--   to construct an empty board for example, there's quite a lot to fill in.

--   \snip{Algebra}{emptyBoard}

--   Writing functions like \Hs{checkWinner} turn out to be even more involved.

--   Rather than going through all of this trouble, we can use our knowledge of the
--   algebra of types to help. The first step is to perform a cardinality analysis on
--   \Ty{TicTacToe};

--   $$
--   \begin{align*}
--     \card{TicTacToe a} &= \underbrace{\card{a} \times \card{a} \times \cdots
--     \times \card{a}}_{9 \text{ times}} \\
--       &= \card{a}^{9} \\
--       &= \card{a}^{3\times 3}
--   \end{align*}
--   $$

--   When written like this, we see that \Ty{TicTacToe} is isomorphic to a function
--   \Ty{(Three, Three) -> a}, or in its curried form: \Ty{Three -> Three -> a}. Of
--   course, \Ty{Three} is any type with three inhabitants; perhaps it looks like
--   this:

--   \snip{Algebra}{Three}

--   Due to this isomorphism, we can instead represent \Ty{TicTacToe} in this form:

--   \snip{Algebra}{TicTacToe2}

--   And thus simplify our implementation of \Hs{emptyBoard}:

--   \snip{Algebra}{emptyBoard2}

--   Such a transformation doesn't let us do anything we couldn't have done
--   otherwise, but it does drastically improve the ergonomics. By making this
--   change, we are rewarded with the entire toolbox of combinators for working with
--   functions; we gain better compositionality and have to pay less of a cognitive
--   burden.

--   Let us not forget that programming is primarily a human endeavor, and ergonomics
--   are indeed a worthwhile pursuit. Your colleagues and collaborators will thank
--   you later!


--   \Section{The Curry--Howard Isomorphism}

--   Our previous discussion of the algebraic relationships between types and their
--   cardinalities can be summarized in the following table.

--   \spaceMyBox
--   \begin{myTable}
--   \hline
--     \myRow{\myCol{\textbf{Algebra}} \myCol{\textbf{Logic}} \myLastCol{\textbf{Types}}} \\ \hline
--     \myRow{\myCol{$a + b$} \myCol{$a \vee b$} \myLastCol{\Ty{Either a b}}} \\ \hline
--     \myRow{\myCol{$a \times b$} \myCol{$a \wedge b$} \myLastCol{\Ty{(a, b)}}} \\ \hline
--     \myRow{\myCol{$b^a$} \myCol{$a \implies b$} \myLastCol{\Ty{a -> b}}} \\ \hline
--     \myRow{\myCol{$a=b$} \myCol{$a \iff b$} \myLastCol{\small\Emph{isomorphism}}} \\ \hline
--     \myRow{\myCol{0} \myCol{$\bot$} \myLastCol{\Ty{Void}}} \\ \hline
--     \myRow{\myCol{1} \myCol{$\top$} \myLastCol{\Ty{()}}} \\ \hline
--   \end{myTable}
--   \spaceMyBox

--   This table itself forms a more-general isomorphism between mathematics and
--   types. It's known as the \Defn{Curry--Howard isomorphism}---loosely stating
--   that every statement in logic is equivalent to some computer program, and vice
--   versa. Curry--Howard has been popularized by Philip Wadler under the name
--   \Defn{propositions as types}.

--   The Curry--Howard isomorphism is a profound insight about our universe. It
--   allows us to analyze mathematical theorems through the lens of functional
--   programming. What's better is that often even ``boring'' mathematical theorems
--   are interesting when expressed as types.

--   To illustrate, consider the theorem $a^1 = a$. When viewed through
--   Curry--Howard, it describes an isomorphism between \Ty{() -> a} and \Ty{a}.
--   Said another way, this theorem shows that there is no distinction between
--   having a value and having a (pure) program that computes that value. This
--   insight is the core principle behind why writing Haskell is such a joy compared
--   with other programming languages.

--   \begin{exercise}
--   Use Curry--Howard to prove the exponent law that $a^b \times a^c =
--   a^{b+c}$. That is, provide a function of the type \Ty{(b -> a) -> (c -> a) ->
--   Either b c -> a} and one of \Ty{(Either b c -> a) -> (b -> a, c -> a)}.
--   \end{exercise}
--   \begin{solution}
--   \unspacedSnip{Algebra}{productRule1To}
--   \snip{Algebra}{productRule1From}

--     Notice that \Hs{productRule1To} is the familiar \Hs{either} function from \Ty{Prelude}.
--   \end{solution}

--   \begin{exercise}
--   Prove $(a\times b)^c = a^c \times b^c$.
--   \end{exercise}
--   \begin{solution}
--   \unspacedSnip{Algebra}{productRule2To}
--   \snip{Algebra}{productRule2From}
--   \end{solution}

--   \begin{exercise}
--   Give a proof of $(a^b)^c = a^{b\times c}$. Does it remind you of
--   anything from \Ty{Prelude}?
--   \end{exercise}
--   \begin{solution}
--   \unspacedSnip{Algebra}{curry}
--   \snip{Algebra}{uncurry}

--     Both of these functions already exist in \Ty{Prelude}.
--   \end{solution}


--   \Section{Canonical Representations}

--   A direct corollary that any two types with the same cardinality are isomorphic,
--   is that there are multiple ways to represent any given type. Although you
--   shouldn't necessarily let it change the way you model types, it's good to keep
--   in mind that you have a choice.

--   Due to the isomorphism, all of these representations of a type are ``just as
--   good'' as any other. However, as we'll see \apageref{ghc.generics}, it's
--   often useful to have a conventional form when working with types generically.
--   This \Defn{canonical representation} is known as a \Defn{sum of products}, and
--   refers to any type \Ty{t} of the form,

--   $$
--   {t = \sum_{m}^{}{\prod_{n}^{}{t_{m,n}}}}
--   $$

--   The big $\Sigma$ means addition, and
--   the $\Pi$ means multiplication---so we can read this as ``addition on the
--   outside and multiplication on the inside.'' We also make the stipulation that
--   all additions must be represented via \Ty{Either}, and that multiplications via
--   \Ty{(,)}. Don't worry, writing out the rules like this makes it seem much more
--   complicated than it really is.

--   All of this is to say that each of following types is in its canonical
--   representation:

--   \begin{itemize}
--     \item{\Ty{()}}
--     \item{\Ty{Either a b}}
--     \item{\Ty{Either (a, b) (c, d)}}
--     \item{\Ty{Either a (Either b (c, d))}}
--     \item{\Ty{a -> b}}
--     \item{\Ty{(a, b)}}
--     \item{\Ty{(a, Int)}---we make an exception to the rule for numeric
--       types, as it would be too much work to express them as sums.}
--   \end{itemize}

--   But neither of the following types are in their canonical representation;

--   \begin{itemize}
--     \item{\Ty{(a, Bool)}}
--     \item{\Ty{(a, Either b c)}}
--   \end{itemize}

--   As an example, the canonical representation of \Ty{Maybe a} is \Ty{Either a ()}.
--   To reiterate, this doesn't mean you should prefer using \Ty{Either a ()} over
--   \Ty{Maybe a}. For now it's enough to know that the two types are equivalent. We
--   shall return to canonical forms in chapter 13.

--   % \todo{WHAT CHAPTER IS GHC GENERICS}

--   \end{document}
--   |]

