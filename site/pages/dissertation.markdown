---
title: Dissertation Project
pin: Dissertation
---

<article>

# Dissertation Project

The following is modified from the slides for a presentation I gave as part of the grade for my dissertation project at University of Glasgow. 
Please forgive the lack of fancy latex embedding :).

The project involved building an interpreter for a simple *session typed* lambda-calculus-like language called GV. 
I chose to write the project in Haskell, which I had little experience with at the time, to give me an excuse to learn the language. 

In hindsight...? Well, I'd say it was a somewhat masochistic choice to learn a new language and paradigm for a project so important to my grade! I kid though, learning Haskell with this project was a great opportunity in my personal CS growth. I now consider myself a lover of Haskell and FP, after all.


You can find the source code for the project on [Github](https://github.com/t1lde/dissertation-project).


---

# Introduction - Motivation

 - Concurrent programming is becoming increasingly relevant, scalability often relies on it.

 - Concurrent programming is **hard**. It is easy to make mistakes when writing concurrent programs, concurrent systems become increasingly complex, leading to a lot of mental overhead.

 - Communication-centric programming often makes things better, but the developer still needs to mentally keep track of *communication protocols*.

 - **Session Types** allow us to specify and enforce communication protocols in the language's type system!


---

# Introduction - The Project


- **GV** is a theoretical programming language which extends the lambda-calculus with concurrent communication primitives and **session types**.

- The goal of the project is to develop an interpreter for GV, which can be used to explore and learn about programming using session types.

- I have developed an interpreter for a simple variant of GV called 'Syncronous GV'.

---

# What are Session Types?

- In message-passing based concurrency, concurrent processes communicate over *channels* .

- Conventionally, channels are given a single type, (or not at all), which very simply restricts what type of data can sent over it.

![An example of a typed channel in Golang](images/go_chan_type.png) 

- Session Types extend the types of channels, allowing more complex behaviour to be specified.

---

# Session Types Example

- This example shows session types for a simple two-factor authentication client and server.

- The Client/Server types are *dual*: Send (!) becomes Recieve (?) and vice-versa.


![Corresponding (dual) session types for a simple server.](images/session_types.png)

- This is a simple example, but session types can specify more complex behaviours.

---

# Syncronous GV

- GV extends the lambda-calculus with concurrent communication and session types.

- In this project, I have implemented a variant of GV called Syncronous GV, or SGV, based on a formal specification of the *syntax*, *typing rules* and *reduction rules*.

- The formal specification is used heavily as a reference, understanding it has been a key part of this project.

---

![SGV Syntax](images/syntax.png)

---


![SGV Types](images/types.png)

---

![SGV Typing Rules](images/typing rules.png)

---

# SGV Reduction Rules

![SGV Reduction Rules](images/reduction.png)

---

# Implementation Overview

- The SGV Interpreter is implemented in Haskell.

- The interpeter is split into three *phases*, parsing, typechecking and execution.

![Interpreter Design](images/phases.png)

---

# Parser Combinators - Megaparsec

- Parser Combinators balance the advantages of manual parser implementations and parser generators.

- Megaparsec is a parser combinator library for Haskell, which allows us to build parsers declaratively from small parts, using operators from Haskell's ``Functor``, ``Monad``, ``Applicative`` and ``Alternative`` typeclasses.

```Haskell
parseLet :: Parser ASTTerm
parseLet =  Let
  <$> ((keyword Sym.keywordLet) *> letNamesTuple)
  <*> ((symbol Sym.equalsSymbol) *> parseTypedTerm)
  <*> ((keyword Sym.keywordIn) *>  parseTypedTerm)
  where
    letNamesTuple = try (inParens parseNames) <|> parseNames
    parseNames = (parseName `sepBy` (symbol Sym.tupleSeparatorSymbol))
```

- Megaparsec forks parsec, and emphasises performance and error handling.

- Megaparsecs error handling is used for parser errors, as well as type errors by annotating the AST with 
  source code positions.
  
```Haskell
  -- Adds annotation to Term parser
withLoc :: Parser ASTTerm -> Parser AST
withLoc p = do
  start <- getOffset
  term <- p
  end <- getOffset
  return $ AST term (TermLoc (start, end))
```
  

![Syntax error](images/syntax_err.png) 

![Type error](images/type_error.png)



---

# Type Checking

- The typechecker is implemented using as a recursive AST traversal.

- The typing context is stored in a State monad, and can be modified to introduce the types of bound variables.

- Linear types are enforced by marking the type of a bound variable as *Used*, and checking the context to make sure a variable is eventually used.

- Type Errors are handled with the ExceptT monad transformer.


```Haskell
-- The Typing rule for lambda application, showing use of checkedAt to specify error annotation
-- positions.
checkTerm (Apply f app) _ = do
  tf <- checkAST f
  tapp <- checkAST app
  (targ, tbody) <- (checkFunction tf tapp) `checkedAt` f
  (matchTypes tapp targ) `checkedAt` app
  return tbody
```

```Haskell
-- The typing rules for lambda abstractions, showing the namesUsed function to check 
-- for linear use of the bound names.
checkTerm (Lambda (TypedName name t locName _) body) _ = do
  tbody <- (ctx,(name,t)) |- checkAST body
  namesUsed [name] `checkedAtLoc` locName
  return $ t -@ tbody
```

---

# Extensible ASTs

- The *Data Types a la Carte* technique is used to allow us to extend the AST with additional Runtime Terms

- Explicit recursion is removed from the AST datatype, and is replaced with a polymorphic type variable.

- A Functor *Fixed Point* is used to bring back the recursion, after extending the AST with extra terms.

```Haskell
type Runtime = Fix (EvalContext
                    :+: Value
                    :+: Para
                    :+: (Const ChannelIdentifier)
                    :+: Partial
                    :+: Term)
```

---

# Fixplate

- The fixplate library provides AST transformation primitives for the *fixed point* AST representaion.

- AST transformations can be defined in small parts, without handling recursion explicitly.

- Combined with *Data Types a la Carte*, we can write polymorphic AST transformations!

```Haskell
subst :: forall sub super. (Functor super, Term :<: super, sub :<: super) => (Fix super) -> (TermIdentifier, sub (Fix super)) -> (Fix super)
subst term (name, replTerm) = transform (subst' name replTerm) term
  where
    subst' :: TermIdentifier -> (sub (Fix super)) -> (Fix super) -> (Fix super)
    subst' name replTerm term = fromMaybe term $ do
      (Id termName) <- match term
      guard (termName == name)
      return $ inject replTerm
```

---

# Project Limitations

- SGV is an extremely restrictive language! Linear types can be hard to deal with.

- There is no recursion, repeating behaviour is impossible to implement!

- Though branching can be implemented with session delegation, this is difficult to work with in practice.

- I didn't have time to do any benchmarking or performance improvement, so the parser especially is extremely slow... If I recall correctly, there's probably some scary space leaks in there. Haskell can be cruel to beginners!


</article>
