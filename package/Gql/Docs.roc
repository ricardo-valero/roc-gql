module [
    Describe,
    describe,
    Deprecate,
    deprecate,
]

Describe implements
    describe : v, Str -> v where v implements Describe

Deprecate implements
    deprecate : v, Str -> v where v implements Deprecate
