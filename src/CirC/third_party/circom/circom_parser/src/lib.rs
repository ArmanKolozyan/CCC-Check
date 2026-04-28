use pest_derive::Parser;
use pest::iterators::Pairs;
use pest::error::Error;
use pest::Parser;
#[derive(Parser)]
#[grammar = "circom.pest"]
struct CircomParser;

pub fn parse(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    CircomParser::parse(Rule::file, input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::*;

    #[test]
    fn examples_dir() {
        use glob::glob;
        use std::fs;
        use std::io::Read;

        // Define the patterns to check
        let patterns = [
            "../circomlib/test/circuits/*.circom",
            "../circomlib/circuits/*.circom",
            "../circomlib/circuits/smt/*.circom",
            "../circomlib/circuits/smt/sha256/*.circom",
            "../../../../circ_dataset/dataset/circom/macro/**/*.circom",
        ];

        // Traverse all .circom files in circomlib
        for pattern in patterns {
            for entry in glob(pattern).expect("Failed to read glob pattern") {
                match entry {
                    Ok(path) => {
                        if path.to_str().unwrap().contains("error") {
                            continue;
                        }

                        println!("Parsing {:?}", path.display());
                        let mut file = fs::File::open(path.clone()).unwrap();

                        let mut data = String::new();
                        file.read_to_string(&mut data).unwrap();

                        match CircomParser::parse(Rule::file, &data) {
                            Ok(_) => {
                                assert!(true);
                            }
                            Err(e) => {
                                println!("Parse error in {}:\n{}", path.display(), e);
                                assert!(false);
                            }
                        }
                    }
                    Err(e) => panic!("{:?}", e),
                }
            }
        }
    }

    #[test]
    fn parse_template_decl() {
        let input = r#"
        template Multiplier(n) {
            signal input a;
            signal input b;
            signal output c;
            c <== a * b;
        }
        "#;
        assert!(CircomParser::parse(Rule::template_decl, input.trim()).is_ok());
    }

    #[test]
    fn parse_postfix_expression() {
        let input = r#"
        template EdDSAPoseidonVerifier() {
            component hash = Poseidon(5);
            hash.inputs[0] <== 0;
        }

        "#;
        assert!(CircomParser::parse(Rule::template_decl, input.trim()).is_ok());
    }

    #[test]
    fn parse_function_decl() {
        let input = r#"
        function factorial(n) {
            var res = 1;
            for (var i = 2; i <= n; i++) {
                res = res * i;
            }
            return res;
        }
        "#;
        assert!(CircomParser::parse(Rule::function_decl, input.trim()).is_ok());
    }

    #[test]
    fn parse_invalid_identifier_because_keyword() {
        fails_with! {
            parser: CircomParser,
            input: "template",
            rule: Rule::identifier,
            positives: vec![Rule::identifier],
            negatives: vec![],
            pos: 0
        };
    }
}
