#include <iostream>
#include <vector>
#include <string>
#include <memory>

using namespace std;

// ================ Token Kind ==================

/// @brief Which kind of Token is
enum TokenType{

    // Variable Type

    Number_Token,

    // General Token

    WhiteSpace_Token,
    Semicolon_Token,
    EndOfLine_Token,

    // Arithmatic Tokens

    Plus_Token,
    Minus_Token,
    Star_Token,
    Slash_Token,

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
        case WhiteSpace_Token: return "WhiteSpace_Token";
        case Semicolon_Token: return "Semicolon_Token";
        case EndOfLine_Token: return "EndOfLine_Token";
        case Plus_Token: return "Plus_Token";
        case Minus_Token: return "Minus_Token";
        case Star_Token: return "Star_Token";
        case Slash_Token: return "Slash_Token";
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

// ================ Laxer ==================

/// @brief For converting Input into Tokens
class Laxer{

private:
    vector<Token> Tokens;
    int pos = 0;
    Token token;
    string Input;
    char currentChar;

    // return's the current character or if needed change offset
    char peek(int offset = 0){
        if((pos + offset) >= Input.length()) {cerr<<"Error : OutOfRange Input String";return '\0';}
        return Input[pos + offset];
    }

    // pos + offset of it increments the pos variable
    char consume(int offset = 1){
        char character = Input[pos];
        pos = pos + offset;
        return character;
    }
public:

    Laxer(string input) : Input(move(input)){
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

            if(isdigit(peek())){
                int _pos = pos;
                int value = GiveInteger();
                Tokens.push_back({_pos,Number_Token,to_string(value),value});
            }

            else if(peek() == '+'){
                Tokens.push_back({pos,Plus_Token,"+",0});         
                consume();       
            }
            else if(peek() == '-'){
                Tokens.push_back({pos,Minus_Token,"-",0});         
                consume();       
            }
            else if(peek() == '*'){
                Tokens.push_back({pos,Star_Token,"*",0});        
                consume();        
            }
            else if(peek() == '/'){
                Tokens.push_back({pos,Slash_Token,"/",0});       
                consume();         
            }

            else if(peek() == ' '){
                consume();            
            }

            else if(peek() == '('){
                Tokens.push_back({pos,Open_Parentheses_Token,"(",0});           
                consume();     
            }
            else if(peek() == ')'){
                Tokens.push_back({pos,Close_Parentheses_Token,")",0});       
                consume();         
            }

            else if(peek() == ';'){
                Tokens.push_back({pos,Semicolon_Token,";",0});    
                consume();            
            }

            else{
                Tokens.push_back({pos,Unknown_Token,"Unknown_TokenType",0});
                consume();
                cerr<<"Error : Unexpected token";
            }
        }
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

// ================ Parser ==================

/// @brief This is For converting or parsering tokens into ast (Abtract Syntax Tree)
class Parser{
private:

    Laxer* laxer;
    vector<Token> Tokens;
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
        else if(match(Open_Parentheses_Token)){
            auto node = parseExpression();
            if(!match(Close_Parentheses_Token)){
                cerr<<"Error : expected ')' at position "<< pos << endl;
            }
            return node;
        }
        else{
            cerr << "Unexpected token at position " << pos << " : " << peek().token << endl;
            consume();
            return nullptr; 
        }
    }

public:
    /// @brief for initiating tokens
    /// @param input code what convert
    Parser(string input){
        laxer = new Laxer(input);
        Tokens = laxer->Tokenizer();

        // Debug token output
        cout << "=== Tokens ===" << endl;
        for (auto& token : Tokens) {
            token.printTokens();
        }
        cout << "==============\n\n\n";
        delete laxer;
    }
    /// @brief for actual parsing tokens into expression
    /// @return whole ast
    unique_ptr<ASTNode> parse(){
        return parseExpression();
    }

};




int main(){
    string input = "2123 + 3134 * 314";

    Parser parser(input);
    unique_ptr<ASTNode> ast = parser.parse();

    cout<<"ast : ";
    
    if (!ast) {
        cout << "AST is null (parse failed)" << endl;
    } else {
        ast->print();
    }
    cout<<endl;

    return 0;
}