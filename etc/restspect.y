%name       restFile
%tokentype  { RestToken }
%error      { parseError }

%token
    Service     { ServiceToken $$ }
    datatype    { DataTypeToken }
    serves      { ServesToken }
    ':'         { ColonToken }
    '`'         { TickToken }

%%

RestApi :   Service serves datatype { RestApi $1 $3 }

{

}
