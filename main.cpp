#include <iostream>
#include <string>
#include <variant>

using namespace std;

#define NULLVARIANT variant<int, std::string>{}


enum TokenType{
    NumberToken,
    WhitespaceToken,
    PlusToken,
    MinusToken,
    SlashToken,
    StarToken,
    OpenParenthesesToken,
    CloseParenthesesToken,
    SemicolonToken,
    EndOfLineToken,
    BadToken
};

const char* TokenTypeToString(TokenType type) {
    switch (type) {
        case NumberToken: return "NumberToken";
        case WhitespaceToken: return "WhitespaceToken";
        case PlusToken: return "PlusToken";
        case MinusToken: return "MinusToken";
        case SlashToken: return "SlashToken";
        case StarToken: return "StarToken";
        case OpenParenthesesToken: return "OpenParenthesesToken";
        case CloseParenthesesToken: return "CloseParenthesesToken";
        case SemicolonToken: return "SemicolonToken";
        case EndOfLineToken: return "EndOfLineToken";
        case BadToken: return "BadToken";
        default: return "UnknownToken";
    }
}


class syntaxToken{
public:
    TokenType Type;
    int Postion;
    string Text;
    variant<int,string> Value;
    
    syntaxToken(TokenType type,int postion,string text,variant<int,string> value){
        Type = type;
        Postion = postion;
        Text = text;
        Value = value;
    }

};

class Laxer{
public:
    string _text;
    int _position;

    Laxer(string text){
        _text = text;
    }

    char current(){
        if(_position >= _text.length()){
            return '\0';
        }
        return _text[_position];
    }

    void next(){
        _position++;
    }

    syntaxToken nextToken(){
        // numbers
        // + - / * ( ) ;
        // whitespace

        if(_position >= _text.length()){
            return syntaxToken(TokenType::EndOfLineToken, _position, "\0", NULLVARIANT);
        }


        if(isdigit(current())){
            int start = _position;
            while(isdigit(current()))
                next();
            
            int length = _position - start;
            string text =  _text.substr(start,length);
            int num = stoi(text);
            return syntaxToken(TokenType::NumberToken, start, text, num);
        }

        if(isspace(current())){
            int start = _position;
            while(isspace(current()))
                next();
            
            int length = _position - start;
            string text =  _text.substr(start,length);
            return syntaxToken(TokenType::WhitespaceToken, start, text, NULLVARIANT);
        }

        if(current() == '+')
            return syntaxToken(TokenType::PlusToken, _position++, "+", NULLVARIANT);
        else if(current() == '-')
            return syntaxToken(TokenType::MinusToken, _position++, "-", NULLVARIANT);
        else if(current() == '*')
            return syntaxToken(TokenType::StarToken, _position++, "*", NULLVARIANT);
        else if(current() == '/')
            return syntaxToken(TokenType::SlashToken, _position++, "/", NULLVARIANT);
        else if(current() == '(')
            return syntaxToken(TokenType::OpenParenthesesToken, _position++, "(", NULLVARIANT);
        else if(current() == ')')
            return syntaxToken(TokenType::CloseParenthesesToken, _position++, ")", NULLVARIANT);
        else if(current() == ';')
            return syntaxToken(TokenType::SemicolonToken, _position++, ";", NULLVARIANT);
        
        return syntaxToken(TokenType::BadToken, _position++, _text.substr(_position - 1, 1), NULLVARIANT);

    }
};


int main(){

    string input;
    getline(cin,input);
    Laxer laxer(input);
    while (true)
    {
        syntaxToken token =  laxer.nextToken();
        if(token.Type == TokenType::EndOfLineToken)
            break;

        cout << TokenTypeToString(token.Type) << " " << token.Text << " ";
        if (holds_alternative<int>(token.Value)) {
            cout << get<int>(token.Value) << " ";
        }
        cout<<endl;
    }
    


    return 0;
}