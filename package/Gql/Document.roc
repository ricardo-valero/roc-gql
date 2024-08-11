module [
    Document,
    Definition,
    OperationType,
    Variable,
    Type,
    Types,
    Selection,
    Argument,
    Directive,
    Value,
    findOperation,
    canSelection,
    CanSelection,
]

Opt value : Result value [Nothing]

Name : Str

# https://spec.graphql.org/October2021/#sec-Document
Document : List Definition

Definition : [
    Operation Operation,
    Fragment Fragment,
]

# 2.3 Operations
Operation : {
    type : OperationType,
    name : Opt Name,
    variables : List Variable,
    directives : List Directive,
    selectionSet : List Selection,
}

OperationType : [Query, Mutation, Subscription]

# 2.4 Selection Sets
Selection : [
    # 2.5 Fields
    Field
        {
            # 2.7 Field Alias
            alias : Opt Name,
            name : Name,
            arguments : List Argument,
            # directives : List Directive,
            selectionSet : List Selection,
        },
    FragmentSpread
        {
            name : Name,
            # directives : List Directive,
        },
    # 2.8.2 Inline Fragments
    InlineFragment
        {
            # 2.8.1 Type Conditions
            typeName : Opt Name,
            # TODO: Directives
            selectionSet : List Selection,
        },
]

# 2.6 Arguments
Argument : (Str, Value)

# 2.8 Fragments
Fragment : {
    name : Name,
    typeName : Str,
    # TODO: Directives
    selectionSet : List Selection,
}

# 2.10 Variables
Variable : {
    name : Str,
    type : Type,
    default : Opt Value,
    directives : List Directive,
}

# 2.11 Type References
Type : [
    Nullable Types,
    NonNull Types,
]

Types : [
    Named Str,
    ListT Type,
]

Directive : (Str, List Argument)

# 2.9 Input Values
Value : [
    Var Str,
    Int I32,
    Float Dec,
    String Str,
    Boolean Bool,
    Null,
    Enum Str,
    List (List Value),
    Object (List (Str, Value)),
]

findOperation : Document, [First, ByName Str] -> Result Operation [OperationNotFound]
findOperation = \doc, rule ->
    state, def <- List.walkUntil doc (Err OperationNotFound)

    when def is
        Operation op ->
            when (op.name, rule) is
                (_, First) ->
                    Break (Ok op)

                (Ok thisName, ByName wantedName) if thisName == wantedName ->
                    Break (Ok op)

                _ ->
                    Continue state

        _ ->
            Continue state

testOp : Operation
testOp = { type: Query, name: Ok "GetUser", variables: [], directives: [], selectionSet: [] }

expect findOperation [] First == Err OperationNotFound
expect findOperation [Operation testOp] (ByName "GetUser") == Ok testOp
expect findOperation [Operation testOp] First == Ok testOp
expect findOperation [Operation testOp] (ByName "getUser") == Err OperationNotFound

findFragment : Document, Str -> Result Fragment [FragmentNotFound]
findFragment = \doc, name ->
    # TODO: Use dict?
    state, def <- List.walkUntil doc (Err FragmentNotFound)

    when def is
        Fragment fragment ->
            if fragment.name == name then
                Break (Ok fragment)
            else
                Continue state

        _ ->
            Continue state

testFragment : Fragment
testFragment = { name: "PostDetails", typeName: "Post", selectionSet: [] }

expect findFragment [] "PostDetails" == Err FragmentNotFound
expect findFragment [Fragment testFragment] "PostDetails" == Ok testFragment
expect findFragment [Fragment testFragment] "Comment" == Err FragmentNotFound

CanSelection : [
    CanField
        {
            name : Str,
            alias : Opt Name,
            arguments : List Argument,
            # TODO: Directives
            selectionSet : List CanSelection,
        },
]

canSelection : Selection, Document -> Result (List CanSelection) [FragmentNotFound Str, RecursiveFragment Str]
canSelection = \sel, doc ->
    canSelectionHelp sel doc (Set.empty {})

canSelectionHelp = \sel, doc, seenFragments ->
    when sel is
        Field field ->
            selections <- field.selectionSet
                |> List.mapTry \subSel -> canSelection subSel doc
                |> Result.map

            [
                CanField {
                    name: field.name,
                    alias: field.alias,
                    arguments: field.arguments,
                    selectionSet: List.join selections,
                },
            ]

        FragmentSpread spread ->
            if Set.contains seenFragments spread then
                Err (RecursiveFragment spread.name)
            else
                fragment <-
                    findFragment doc spread.name
                    |> Result.mapErr \FragmentNotFound -> FragmentNotFound spread.name
                    |> Result.try

                newSeenFragments =
                    seenFragments |> Set.insert spread

                fragment.selectionSet
                |> List.mapTry \subSel -> canSelectionHelp subSel doc newSeenFragments
                |> Result.map List.join

        _ ->
            crash "todo"

expect
    doc = [
        Fragment {
            name: "Post",
            typeName: "Post",
            selectionSet: [
                Field { name: "body", alias: Err Nothing, arguments: [], selectionSet: [] },
                Field {
                    name: "author",
                    alias: Err Nothing,
                    arguments: [],
                    selectionSet: [FragmentSpread { name: "User" }],
                },
            ],
        },
        Fragment {
            name: "User",
            typeName: "User",
            selectionSet: [
                Field { name: "name", alias: Err Nothing, arguments: [], selectionSet: [] },
            ],
        },
    ]

    sel = Field {
        name: "posts",
        alias: Err Nothing,
        arguments: [],
        selectionSet: [
            Field { name: "title", alias: Err Nothing, arguments: [], selectionSet: [] },
            FragmentSpread { name: "Post" },
        ],
    }

    expected = [
        CanField {
            name: "posts",
            alias: Err Nothing,
            arguments: [],
            selectionSet: [
                CanField { name: "title", alias: Err Nothing, arguments: [], selectionSet: [] },
                CanField { name: "body", alias: Err Nothing, arguments: [], selectionSet: [] },
                CanField {
                    name: "author",
                    alias: Err Nothing,
                    arguments: [],
                    selectionSet: [
                        CanField { name: "name", alias: Err Nothing, arguments: [], selectionSet: [] },
                    ],
                },
            ],
        },
    ]

    canSelection sel doc == Ok expected

expect
    doc = [
        Fragment {
            name: "Post",
            typeName: "Post",
            selectionSet: [FragmentSpread { name: "Post" }],
        },
    ]

    sel = Field {
        name: "posts",
        alias: Err Nothing,
        arguments: [],
        selectionSet: [FragmentSpread { name: "Post" }],
    }

    canSelection sel doc == Err (RecursiveFragment "Post")

# TODO: Figure out why this stack overflows:
# expect
#    doc = [
#        Fragment {
#            name: "User",
#            typeName: "User",
#            selectionSet: [
#                Field { name: "posts", alias: Err Nothing, arguments: [], selectionSet: [FragmentSpread { name: "Post" }] },
#            ],
#        },
#        Fragment {
#            name: "Post",
#            typeName: "Post",
#            selectionSet: [
#                Field { name: "author", alias: Err Nothing, arguments: [], selectionSet: [FragmentSpread { name: "User" }] },
#            ],
#        },
#    ]

#    sel = Field { name: "posts", alias: Err Nothing, arguments: [], selectionSet: [FragmentSpread { name: "Post" }] }

#    canSelection sel doc == Err (RecursiveFragment "Post")
