#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <fstream>
#include <sstream>
#include <unordered_map>

using namespace std;

// ================ Token Kind ==================

/// @brief Which kind of Token is
enum TokenType{

    // Variable Token

    Number_Token,
    
    // Keywords Token

    Let_Token,

    // General Token

    Semicolon_Token,
    EndOfLine_Token,
    Identifier_Token,

    // Arithmatic Tokens

    Plus_Token,
    Minus_Token,
    Star_Token,
    Slash_Token,

    // Assignment Tokens

    Equal_Token,

    // Brakets Tokens

    Open_Parentheses_Token,
    Close_Parentheses_Token,

    // Unknown Tokens

    Unknown_Token,
};

/// @brief This function is like util for debug that converts enum if TokenType into String so we can easy print it
/// @param type TokenType
/// @return TokenType into String
string TokenTypeToString(TokenType type) {
    switch (type) {
        case Number_Token: return "Number_Token";
        case Semicolon_Token: return "Semicolon_Token";
        case EndOfLine_Token: return "EndOfLine_Token";
        case Plus_Token: return "Plus_Token";
        case Minus_Token: return "Minus_Token";
        case Star_Token: return "Star_Token";
        case Slash_Token: return "Slash_Token";
        case Equal_Token: return "Equal_Token";
        case Let_Token: return "Let_Token";
        case Open_Parentheses_Token: return "Open_Parentheses_Token";
        case Close_Parentheses_Token: return "Close_Parentheses_Token";
        case Unknown_Token: return "Unknown_Token";
        default: return "Error : Unknown_TokenType";
    }
}

// ================ Token Structure ==================

/// @brief This is structure for token
struct Token
{
    int position;
    TokenType tokenType;
    string token;
    int value;


    /// @brief Debug Util : prints given Token
    /// @param token Token
    void printTokens(){  
        cout<< position << ") "<<TokenTypeToString(tokenType) << " : " << token << endl;
    }
};

// ================ Lexer ==================

/// @brief For converting Input into Tokens
class Lexer{

private:
    vector<Token> Tokens;
    int pos = 0;
    Token token;
    string Input;

    unordered_map<char,TokenType> singleCharToken = {
        {'+', Plus_Token},
        {'-', Minus_Token},
        {'*', Star_Token},
        {'/', Slash_Token},
        {'=', Equal_Token},
        {'(', Open_Parentheses_Token},
        {')', Close_Parentheses_Token},
        {';', Semicolon_Token}
    };

    unordered_map<string,TokenType> keywordToken = {
        {"let",Let_Token}
    };


    // Return the current character with optional lookahead
    char peek(int offset = 0) {
        if ((pos + offset) >= Input.size()) {
            return '\0';  // Safe EOF sentinel
        }
        return Input[pos + offset];
    }

    // Advance the position and return the current character
    char consume(int offset = 1) {
        if ((pos + offset - 1) >= Input.size()) {
            cerr << "Error: Tried to consume past end of input at pos = " << pos 
                 << ", offset = " << offset << ", size = " << Input.size() << endl;
            throw runtime_error("Lexer::consume out of bounds");
        }
        char character = Input[pos];
        pos += offset;
        return character;
    }


    // check's first letter of identifier
    bool checkIdentifierStart(char ch){
        return isalpha(ch) || ch == '_';
    }

    bool checkIdentifierBody(char ch){
        return isalnum(ch) || ch == '_';
    }
public:

    Lexer(string input) : Input(move(input)){
        // if needed add later
    }

    /// @brief This method reads the input char by char and converts into whole integer
    /// @return Convert char by char digits into whole integer
    int GiveInteger(){
        int result = 0;
        while(isdigit(peek())){
            result = result * 10 + (consume() - '0');
        }
        return result;
    }

    /// @brief This function is for converting string into each tokens
    /// @return Vector of Tokens
    vector<Token> Tokenizer(){
        while(peek() != '\0'){
            char ch = peek();
            if(isdigit(ch)){
                int _pos = pos;
                int value = GiveInteger();
                Tokens.push_back({_pos,Number_Token,to_string(value),value});
            }

            else if(singleCharToken.count(ch)){
                Tokens.push_back({pos,singleCharToken[ch],string(1,ch),0});         
                consume();       
            }

            else if(checkIdentifierStart(ch)){
                int _pos = pos;
                string id;
                while (checkIdentifierBody(peek())){
                    id += consume();
                }
                if(keywordToken.count(id)){
                    Tokens.push_back({_pos,keywordToken[id],id,0});
                }else{
                    Tokens.push_back({_pos,Identifier_Token,id,0});
                }
            }

            else if(isspace(ch)){
                consume();            
            }

            else{
                Tokens.push_back({pos,Unknown_Token,string(1,ch),0});
                cerr<<"Error : Unexpected token" << ch << " position " << pos <<endl;
                consume();
            }
        }
        Tokens.push_back({pos,EndOfLine_Token,"EOfL",0});
        return Tokens;
    }
};


// ================ AST Node Stuff ==================

/// @brief base of ast node
struct ASTNode{
    virtual ~ASTNode() {};
    virtual void print() const = 0;
};

/// @brief ast node that stores number
struct NumberNode : ASTNode{
    int value;
    NumberNode(int val) : value(val){};
    void print() const override {cout<<value;}
};

/// @brief ast node that stores one binary expression
struct BinaryOperatorNode : ASTNode{
    char op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    BinaryOperatorNode(ASTNode* l,char o,ASTNode* r) : left(move(l)),op(o),right(move(r)) {};

    void print() const override{
        cout<< "( "; 
        left->print();
        cout<< " " << op << " ";
        right->print();
        cout << " )";
    }
};

/// @brief ast node thar stores variable name
struct VariableNameNode : ASTNode{
    string name;
    VariableNameNode(const string& n) : name(n) {}
    void print() const override{
        cout<<name;
    } 
};

/// @brief stores whole varible with value
struct AssignmentNode : ASTNode{
    string variableName;
    unique_ptr<ASTNode> value;
    AssignmentNode(const string& name,ASTNode* val): variableName(name),value(val){};
    void print() const override{
        cout << variableName << " = ";
        value->print();
    }
};




// ================ Parser ==================

/// @brief This is For converting or parsering tokens into ast (Abtract Syntax Tree)
class Parser{
private:

    unique_ptr<Lexer> lexer;
    vector<Token> Tokens;
    unordered_map<string,int> variables;
    int pos = 0;

    /// @brief For accessing next token
    /// @return Next Token
    Token peek(int offset = 0){
        if (pos + offset >= Tokens.size()) {
            return {pos + offset, EndOfLine_Token, "", 0};
        }
        return Tokens[pos + offset];
    }

    /// @brief consumes current token and increments the position
    /// @return A previous token 
    Token consume(){
        if(pos < Tokens.size()) pos++;
        return Tokens[pos - 1];
    }
    /// @brief For comparing the given tokentype with current position token
    /// @param expected compare givens current token 
    /// @return true or false based on if matched or not
    bool match(TokenType expected){
        if(peek().tokenType == expected){
            consume();
            return true;
        }
        return false;
    }

    unique_ptr<ASTNode> parseStatement(){
        if(peek().tokenType == Let_Token && peek(1).tokenType == Identifier_Token && peek(2).tokenType == Equal_Token){
            consume(); // let token
            string variableName = consume().token;
            consume(); // =
            auto expr = parseExpression();
            return make_unique<AssignmentNode>(variableName,expr.release());
        }
        return parseExpression();
    }

    /// @brief for making + and - token into binary expression
    /// @return a full binary expression with operator + or -
    unique_ptr<ASTNode> parseExpression(){
        auto node = parseTerm();
        while(peek().tokenType == Plus_Token || peek().tokenType == Minus_Token){
            char op = peek().token[0];
            consume();
            auto right = parseTerm();
            node = make_unique<BinaryOperatorNode>(node.release(),op,right.release());
        }
        return node;
    }

    /// @brief for making * and / token into binary expression
    /// @return a full binary expression with operator * or /
    unique_ptr<ASTNode> parseTerm(){
        auto node = parseFactor();
        while(peek().tokenType == Star_Token || peek().tokenType == Slash_Token){
            char op = peek().token[0];
            consume();
            auto right = parseFactor();
            node = make_unique<BinaryOperatorNode>(node.release(),op,right.release());
        }
        return node;
    }
    

    /// @brief used for making or calling different function and making valid ast
    /// @return a number node or whole binary expression
    unique_ptr<ASTNode> parseFactor(){
        if(match(Number_Token)){
            return make_unique<NumberNode>(Tokens[pos - 1].value);
        }
        else if (match(Identifier_Token)) {
            return make_unique<VariableNameNode>(Tokens[pos - 1].token);
        }
        else if(match(Open_Parentheses_Token)){
            auto node = parseExpression();
            if(!match(Close_Parentheses_Token)){
                cerr<<"Error : expected ')' at position "<< pos << endl;
            }
            return node;
        }
        else{
            cerr << "Unexpected token at position " << pos << " : " << peek().token << endl;
            throw runtime_error("Error: Unexpected token");
        }
    }

public:
    /// @brief for initiating tokens
    /// @param input code what convert
    Parser(string input){
        lexer = make_unique<Lexer>(input);
        Tokens = lexer->Tokenizer();


        // Debug token output
        {   
            cout << "=== Tokens ===" << endl;
            for (auto& token : Tokens) {
                token.printTokens();
            }
            cout << "==============\n\n\n";
        }
    }
    /// @brief for actual parsing tokens into expression
    /// @return whole ast
    vector<unique_ptr<ASTNode>> parse(){

        vector<unique_ptr<ASTNode>> lines;
        
        while(peek().tokenType != EndOfLine_Token){
            if (peek().tokenType == Semicolon_Token) {
                // Empty statement consume semicolon and continue
                consume();
                continue;
            }   

            unique_ptr<ASTNode> expr = parseStatement();
            if(!expr){cerr<<"Error : problem when parsing expression";}

            if(!match(Semicolon_Token)){
                cerr << "Error : expected ';' at " << pos <<endl;
                return {};
            }
            lines.push_back(move(expr));
        }

        return lines;
    }

    /// @brief It evaluates AST  
    /// @param node head of ast 
    /// @return evaluated AST answer
    int evaluateAST(const ASTNode* node){
        if(const NumberNode* n = dynamic_cast<const NumberNode*>(node)){
            return n->value;
        }
        else if(const VariableNameNode* vnn = dynamic_cast<const VariableNameNode*>(node)){
            if(variables.count(vnn->name)){
                return variables[vnn->name];
            } else {
                throw runtime_error("Error: Undefined variable " + vnn->name);
            }
        }
        else if(const AssignmentNode* an = dynamic_cast<const AssignmentNode*>(node)){
            int val = evaluateAST(an->value.get());
            variables[an->variableName] = val;
            return val;
        }
        else if(const BinaryOperatorNode* bon = dynamic_cast<const BinaryOperatorNode*>(node)){
            int leftVal = evaluateAST(bon->left.get());
            int rightVal = evaluateAST(bon->right.get());

            switch (bon->op){   
            case '+': return leftVal + rightVal;
            case '-': return leftVal - rightVal;
            case '*': return leftVal * rightVal;
            case '/':                 
                if (rightVal == 0) {
                    throw runtime_error("Error: Division by zero");
                }
                return leftVal / rightVal;
            }
        }
        throw runtime_error("Error : Unknown node");
    }

};

// ========== File Management Functions ============

string readFile(string filename){
    ifstream file(filename);
    if(!file){
        cerr << "There was problem in reading files";
        return "";
    }

    stringstream out;
    out << file.rdbuf();
    return out.str();
}

// ================ Main Function ==================

int main(int argc,char* argv[]){
    
    if(argc < 2 ){
        cerr<< "Error : CLI Argument" << endl;
        return 1;
    }

    string input = readFile(argv[1]);
    
    // string input = "2123 + 3134 * 314";
    Parser parser(input);
    vector<unique_ptr<ASTNode>> lines = parser.parse();

    for(const auto& ast : lines){
        
        cout<<"ast : ";
        if (!ast) {
            cout << "AST is null (parse failed)" << endl;
        } else {
            ast->print();
        }
        cout<<endl;
        cout << "evaluated ast result :" << parser.evaluateAST(ast.get())<<endl;
    }
    return 0;
}