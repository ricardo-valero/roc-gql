module [andThen]

import parser.Core exposing [
    Parser,
    parsePartial,
    buildPrimitiveParser,
]

# Internal utility function. Not exposed to users, since usage is discouraged!
#
# Runs `firstParser` and (only) if it succeeds,
# runs the function `buildNextParser` on its result value.
# This function returns a new parser, which is finally run.
#
# `andThen` is usually more flexible than necessary, and less efficient
# than using `const` with `map` and/or `apply`.
# Consider using those functions first.
andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \firstParser, buildNextParser ->
    fun = \input ->
        { val: firstVal, input: rest } <- Result.try (parsePartial firstParser input)
        nextParser = buildNextParser firstVal

        parsePartial nextParser rest

    buildPrimitiveParser fun
