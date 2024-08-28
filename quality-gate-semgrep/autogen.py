from ast_grep_py import (
    SgRoot,
    Rule,
    Config,
    Pattern,
    Relation,
    register_dynamic_language,
)
import yaml
from os import listdir
from os.path import isfile, join, splitext
from pprint import pp


def main():
    register_dynamic_language(
        {
            "ocaml-grammar": {
                "library_path": "../ocaml.so",
                "language_symbol": "tree_sitter_ocaml",
            }
        }
    )

    files = [f for f in listdir("./shortrules/") if isfile(join("./shortrules/", f))]

    # Generate complete rule files from short OCaml snippets
    for rule_file in files:
        short_name = splitext(rule_file)[0]
        print(f"Generating a rule for {short_name}")

        source = ""
        replace_pattern = ""
        still_pattern = True
        with open(join("./shortrules/", rule_file), "r") as f:
            for line in f:
                line = line.strip()
                if line.startswith("(*"):
                    still_pattern = False
                    continue

                if still_pattern:
                    source += line
                else:
                    replace_pattern += line

        # Parse the AST of the OCaml snippet
        sg = SgRoot(source, "ocaml-grammar")
        node = sg.root()

        # Skip compilation_unit and expression_item to get
        # to the actual expression
        while node.kind() in ["compilation_unit", "expression_item"]:
            node = node.child(0)

        # Construct the ast-grep rule
        # Does the pattern feature modules?
        module_node = node.find(
            **Rule(has=Relation(Rule(kind="module_path"), stopBy="end"))
        )
        if module_node is not None:
            module_name = module_node.text()
            print(f"\t(with module = '{module_name}')")
            # If so, these need to be handled in a special way, since:
            # They can modify the scope through `open Module`, `include Module`,
            # and can be renamed like `module X = Module`
            rules = gen_matches(node)
            rules.append(gen_module_check(module_name))
            conf = {
                "id": short_name,
                "language": "ocaml",
                "severity": "error",
                "rule": {"kind": node.kind(), "all": rules},
                "fix": replace_pattern,
                "utils": gen_utils(module_name),
            }
        else:
            conf = {
                "id": splitext(rule_file)[0],
                "language": "ocaml",
                "severity": "error",
                "rule": {"kind": node.kind(), "all": gen_matches(node)},
                "fix": replace_pattern,
            }

        # Dump the YAML rule into a file
        with open(join("./rules/", f"{short_name}.yml"), "w") as f:
            source = f.write(yaml.dump(conf, sort_keys=False))
        print(f"\tgenerated rule file './rules/{short_name}.yml'")


# kinds that need to be treated specially - dot seems to be some kind of an
# error so it's excluded, while modules need to be matched for in more complex
# ways
special_kinds = [".", "module_path"]


# Goes over the provided shortrule AST - and creates match rules
# every node. In particular, it matches for an identical 'kind'
# and regex.
def gen_matches(n, module_name=None):
    return [
        (
            Rule(has=Rule(kind=child.kind(), all=child_rules))
            if (len(child_rules) > 1)
            else Rule(
                has=Rule(kind=child.kind(), nthChild=i + 1, has=child_rules[0]["has"])
            )
        )
        if (child_rules := gen_matches(child))
        else (
            Rule(has=Rule(kind=child.kind(), pattern="$VAR_NAME"))
            if child.text() == "_"
            else Rule(has=Rule(kind=child.kind(), regex=child.text()))
        )
        for i, child in enumerate(n.children())
        if child.kind() not in special_kinds
    ]


# Checks if the function being applied somehow comes from provided module
def gen_module_check(module_name):
    return Rule(
        any=[
            # checks if it's a simple immediate 'List.length'
            Rule(
                has=Relation(Rule(kind="module_path", regex=module_name), stopBy="end")
            ),
            # or if the module in the immediate application was renamed
            Rule(has=Relation(Rule(matches="is-renamed"), stopBy="end")),
            # checks if the match is inside a block (however many levels in)
            # which in one way or another has 'List' open at the top level
            Rule(
                inside=Relation(
                    Rule(
                        has=Rule(
                            any=[
                                # check if the function is inside the scope of an 'open List'
                                Rule(
                                    kind="open_module",
                                    has=Rule(kind="module_path", regex=module_name),
                                ),
                                # check if the function is used inside a local open expr
                                # like: List.( length ... )
                                Rule(
                                    kind="local_open_expression",
                                    has=Rule(kind="module_path", regex=module_name),
                                ),
                                # check if the module being used was renamed
                                Rule(matches="is-renamed"),
                            ]
                        ),
                        stopBy="end",
                    )
                ),
            ),
        ]
    )


def gen_utils(module_name):
    return {
        "is-renamed": Rule(
            all=[
                Rule(kind="module_path", pattern="$MODNAME"),
                Rule(
                    inside=Relation(
                        Rule(
                            has=Rule(
                                kind="module_definition",
                                has=Rule(
                                    kind="module_binding",
                                    all=[
                                        Rule(
                                            has=Rule(
                                                kind="module_name", pattern="$MODNAME"
                                            )
                                        ),
                                        Rule(
                                            has=Rule(
                                                kind="module_path",
                                                has=Rule(
                                                    kind="module_name",
                                                    regex=module_name,
                                                ),
                                            )
                                        ),
                                    ],
                                ),
                            )
                        ),
                        stopBy="end",
                    )
                ),
            ]
        )
    }


if __name__ == "__main__":
    main()
