#include "lexer.h"
#include "parser.h"
#include "schemeLLVM.h"

void prettyPrintTokenArray(std::vector<Token*> tokens) {
    for (auto it = tokens.begin(); it < tokens.end(); it++) {
        switch ((*it)->tokenType) {
            case TokenType::Let: {
                std::cout << "Let"
                          << "\n";
                break;
            }
            case TokenType::LParen: {
                std::cout << "LParen"
                          << "\n";
                break;
            }
            case TokenType::RParen: {
                std::cout << "RParen"
                          << "\n";
                break;
            }
            case TokenType::Identifier: {
                std::cout << "Identifier: " << (*it)->identifierValue << "\n";
                break;
            }
            case TokenType::String: {
                std::cout << "String: " << (*it)->stringValue << "\n";
                break;
            }
            case TokenType::Number: {
                std::cout << "Number: " << (*it)->numValue << "\n";
                break;
            }
            case TokenType::LSquare: {
                std::cout << "["
                          << "\n";
                break;
            }
            case TokenType::RSquare: {
                std::cout << "]"
                          << "\n";
                break;
            }
            case TokenType::Semicolon: {
                std::cout << ";"
                          << "\n";
                break;
            }
            case TokenType::Plus: {
                std::cout << "+"
                          << "\n";
                break;
            }
            case TokenType::Minus: {
                std::cout << "-"
                          << "\n";
                break;
            }
            case TokenType::Equal: {
                std::cout << "="
                          << "\n";
                break;
            }
            case TokenType::Comma: {
                std::cout << ","
                          << "\n";
                break;
            }
        }
    }
}

int main(int argc, char* argv[]) {
    std::string program = R"(
    let hello = 3 + 2;
    defun addone(x){
      x+1;
    }
  )";
    SchemeLLVM vm;
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    for (auto token = tokens.begin(); token < tokens.end(); token++) {
        std::cout << "Lexer token " << (*token)->tokenType << "\n";
    }
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    auto statements = programNode->statements;
    assert((statements[0].get()->type == StatementType::Let) &&
           "Let statement expected");
    assert(statements[1].get()->type == StatementType::Expression &&
           "Let statement expected");
    assert(statements[0].get()->letStatement->value->type ==
               ExpressionType::Infix &&
           "Infix expression expected");

    // vm.exec(program);
    return 0;
}
