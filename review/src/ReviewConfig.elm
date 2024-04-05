module ReviewConfig exposing (config)

import NoDeprecated
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoLongImportLines
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnmatchedUnit
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


{-| `elm-review` config
-}
config : List Rule
config =
    [ -- Forbid use of deprecated functions
      NoDeprecated.rule NoDeprecated.defaults

    -- Forbid `module A exposing (..)`
    , NoExposingEverything.rule

    -- Forbid `import A exposing (..)`
    , NoImportingEverything.rule []

    -- Ensure consistent import aliases and enforce their use
    , NoInconsistentAliases.config []
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule

    -- Forbid import lines longer than 120 characters
    , NoLongImportLines.rule

    -- Forbid missing type annotations for TLDs
    , NoMissingTypeAnnotation.rule

    -- Forbid missing type annotations in let expressions
    , NoMissingTypeAnnotationInLetIn.rule

    -- Forbid not exposing the type for any types that appear in exported functions or values
    , NoMissingTypeExpose.rule
        |> Rule.ignoreErrorsForFiles [ "src/Main.elm" ]

    -- Disallow qualified use of names imported unqualified
    , NoModuleOnExposedNames.rule

    -- Forbid let declarations that are computed earlier than needed
    , NoPrematureLetComputation.rule

    -- Forbid `let a = 5 in`
    , NoSimpleLetBody.rule

    -- Disallow matching `()` with `_`
    , NoUnmatchedUnit.rule

    -- Forbid recursion without TCO
    , NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO"
        |> NoUnoptimizedRecursion.rule

    -- Report unused custom type constructors
    , NoUnused.CustomTypeConstructors.rule []

    -- Report unused custom type fields
    , NoUnused.CustomTypeConstructorArgs.rule

    -- Report unused dependencies
    -- can't do this because we'll yank out elm/html, which lamdera needs under the covers
    --, NoUnused.Dependencies.rule
    -- Report exports never used in other modules
    , NoUnused.Exports.rule

    -- Report modules never used (or exported in the package)
    , NoUnused.Modules.rule

    -- Report unused function parameters
    , NoUnused.Parameters.rule

    -- Report unused parameters in pattern matching
    , NoUnused.Patterns.rule

    -- Report variables that are declared but never used
    -- , NoUnused.Variables.rule
    -- Detect simplifiable expressions, e.g. `a == True` can be simplified to `a`
    , Simplify.rule Simplify.defaults

    -- Enforce naming in camelCase and PascalCase
    ]
