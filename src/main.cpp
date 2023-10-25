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

void testParseLetStatement() {
    std::string program = R"(
      let hello = 3 - 2;
  )";
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
}
void testParseFunction() {
    std::string program = R"(
    defun main(){
        let hello = 3 - 2;
    }
  )";
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    auto statements = programNode->statements;
    assert(statements[0]->type == StatementType::Function &&
           "Function statement expected");
    assert(statements[0]->funcStatement->name == "main" &&
           "Function name main expected");
    assert(statements[0]->funcStatement->body->statements[0]->type ==
               StatementType::Let &&
           "Let statement expected");
    assert(statements[0]
                   ->funcStatement->body->statements[0]
                   ->letStatement->value->type == ExpressionType::Infix &&
           "Infix expression expected");
    assert(*(statements[0]
                 ->funcStatement->body->statements[0]
                 ->letStatement->identifier) == "hello" &&
           "Identifier hello expected");
}

void testParseCall() {
    std::string program = R"(
    defun add(a: int, b: int): int{
         return a + b;
    }
    let five = add(3,2);
  )";
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    auto statements = programNode->statements;
    assert(statements[1]->type == StatementType::Let && "Expected let");
    assert(*(statements[1]->letStatement->identifier) == "five" &&
           "Expected five");
    assert(statements[1]->letStatement->value->type == ExpressionType::Call &&
           "Expected call expression");
    assert(*(statements[1]->letStatement->value->callExpression->fnName) ==
               "add" &&
           "Expected sum function name");
    assert(statements[1]
                   ->letStatement->value->callExpression->arguments[0]
                   ->integerExp->intValue == 3 &&
           "Expected 3");
    assert(statements[1]
                   ->letStatement->value->callExpression->arguments[1]
                   ->integerExp->intValue == 2 &&
           "Expected 3");
}

void testIfStatement() {
    std::string program = R"(
    defun add(a: int, b: int): int{
         return a + b;
    }
    if(3 > 2){
      printf("3 is greater than 2");
    }else{
      printf("3 is less than 2");
    }
  )";
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    auto statements = programNode->statements;
    assert(statements[1]->type == StatementType::If && "Expected IF Statement");
    assert(statements[1]->ifStatement->condition->type ==
               ExpressionType::Infix &&
           "Expected infix for condition");
    assert(statements[1]->ifStatement->condition->infixExpr->op ==
               TokenType::GreaterThan &&
           "Expected > operation");
    assert(statements[1]
                   ->ifStatement->condition->infixExpr->lhs->integerExp
                   ->intValue == 3 &&
           "Expected 3");
    assert(statements[1]
                   ->ifStatement->condition->infixExpr->rhs->integerExp
                   ->intValue == 2 &&
           "Expected 2");
    assert(*(statements[1]
                 ->ifStatement->trueBlock->statements[0]
                 ->expressionStatement->expression->callExpression->fnName) ==
               "printf" &&
           "Expected printf");
    assert(*(statements[1]
                 ->ifStatement->falseBlock->statements[0]
                 ->expressionStatement->expression->callExpression->fnName) ==
               "printf" &&
           "Expected printf");
}

void testParseFor() {
    std::string program = R"(
    defun add(a: int, b: int): int{
         return a + b;
    }
    for(let i = 0;i < 10;i = i+1){
      printf("%d",i);
    }
  )";
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    auto statements = programNode->statements;
    assert(statements[1]->type == StatementType::For &&
           "Expected for statement");
    assert(*(statements[1]
                 ->forStatement->initialCondition->letStatement->identifier) ==
               "i" &&
           "Expected i");
    assert(statements[1]
                   ->forStatement->initialCondition->letStatement->value
                   ->integerExp->intValue == 0 &&
           "Expected 0");
    assert(*(statements[1]
                 ->forStatement->continueCondition->infixExpr->lhs
                 ->identifierExpression->name) == "i" &&
           "Expected i");
    assert(statements[1]
                   ->forStatement->continueCondition->infixExpr->rhs->integerExp
                   ->intValue == 10 &&
           "Expected 0");
    assert(*(statements[1]
                 ->forStatement->updateCondition->expressionStatement
                 ->expression->infixExpr->lhs->identifierExpression->name) ==
               "i" &&
           "Expected i");
    assert(
        statements[1]
                ->forStatement->updateCondition->expressionStatement->expression
                ->infixExpr->rhs->infixExpr->rhs->integerExp->intValue == 1 &&
        "Expected 1");
    assert(*(statements[1]
                 ->forStatement->body->statements[0]
                 ->expressionStatement->expression->callExpression->fnName) ==
               "printf" &&
           "Expected printf");
    assert(statements[1]->forStatement->continueCondition->infixExpr->op ==
               TokenType::LessThan &&
           "Expected less than in infix for continue condition");
    assert(statements[1]
                   ->forStatement->updateCondition->expressionStatement
                   ->expression->infixExpr->op == TokenType::Equal &&
           "Expected equal in infix for update condition");
}

int main(int argc, char* argv[]) {
    std::string program = R"(
      defun add(a: int, b: int): int{
           return a + b;
      }
      for(let i = 0;i < 10;i = i+1){
          printf("%d\n",add(i,1));
      }
    )";
    testParseFor();
    SchemeLLVM vm;
    vm.exec(program);
    return 0;
}
