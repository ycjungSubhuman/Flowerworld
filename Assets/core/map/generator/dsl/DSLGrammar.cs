using UnityEngine;
using System.Collections.Generic;
using System.Linq;
using Sprache;
using Core.Map;

namespace Core.Map.Generator.DSL
{
    /**
     * 문법)
     * 
     * (작은 따옴표 안에 있는 것은 실제 텍스트가 아닌 문법을 정의하기 위한 심볼이다)
     * (+는 하나 또는 그 이상의 회수만큼 반복, *은 0 또는 그 이상의 회수만큼 반복을 의미한다.)
     * (A | B는 A 또는 B를 의미한다.)
     * 
     * 'map_statement' where
     * 
     * 'empty'              ::= 아무 글자도 없는 텍스트
     * 'newline'            ::= \n | \r\n
     * 'whitespace'         ::= 스페이스바 | 탭(\t)
     * 'symbol'             ::= START | A | B | C | D | E | F | G | ANY | GOAL
     * 'map_type'          ::= NORMAL | CONSTRUCT | DOGDE
     * 'cell'               ::= 'empty' | 'cell','symbol'
     * 'row'                ::= 'empty' | 'row'('whitespace'*)'cell'('whitespace'*)"|"
     * 'block_statement'    ::= block('whitespace'*)('newline'+)('row''whitespace'*'newline')+(('newline' | 'whitespace)*)
     * 'title_statement'    ::= title 'string''newline'
     * 'gametype_statement' ::= type 'map_type''newline'
     * 'map_statement'      ::= 'block_statement'+
     * 'stage_statement'    ::= 'title_statement''gametype_statement''map_statement'
     * 
     * eg)
     * 
     * block
     * 
     *     START,ANY |  B   |   A   |   B   |   A      |
     *         B    |  C    |   B   |   A   |   A        |
     *         A    |  C    |       |     
     *              |  A    |   B   |   C   |            |
     *              |       |   A   |   B   |   C,GOAL   |
     *              
     * block
     * 
     *     ...
     * 
     * 유의사항)
     * 1. 블럭이 여러 개 정의될 경우, 가장 먼저 정의된 block이 메인 맵으로 정해진다. 나머지는 드래그 가능한 서브 맵으로 할당된다.
     * 2. 아무 것도 없는 빈 row는 row로 여겨지지 않는다.
     * 3. 각 row의 길이는 똑같지 않아도 된다.(최대 길이로 자동으로 맞춰짐)
     */
    public class DSLGrammar
    {

        static readonly Parser<string> TitleStatement =
            from titleStatementIdentifier in Parse.String ("title")
            from whitespace in Parse.WhiteSpace.AtLeastOnce ()
            from title in Parse.LetterOrDigit.Or(Parse.Char('-')).AtLeastOnce ().Text ()
            from linebreak in Parse.LineEnd
            select title;

        static readonly Parser<Map.Type> MapType =
            Parse.String ("NORMAL").Return (Map.Type.NORMAL)
            .Or (Parse.String ("CONSTRUCT").Return (Map.Type.CONSTRUCT))
            .Or (Parse.String ("DODGE").Return (Map.Type.DODGE));

        static readonly Parser<Map.Type> MapTypeStatement =
            from prev in Parse.LineEnd.Or(Parse.WhiteSpace.Many().Text()).Many()
            from typeStatementIdentifier in Parse.String ("type")
            from type in MapType
            select type;

        static readonly Parser<char> LabelSeparator = Parse.Char (',').Token ();
        static readonly Parser<char> CellSeparator = Parse.Char ('|').Token ();

        static readonly Parser<MapBlock.Cell.Label> Symbol =
            Parse.String ("START").Return (MapBlock.Cell.Label.START)
            .Or (Parse.String ("ANY").Return (MapBlock.Cell.Label.ANY))
            .Or (Parse.String ("A").Return (MapBlock.Cell.Label.A))
            .Or (Parse.String ("B").Return (MapBlock.Cell.Label.B))
            .Or (Parse.String ("C").Return (MapBlock.Cell.Label.C))
            .Or (Parse.String ("D").Return (MapBlock.Cell.Label.D))
            .Or (Parse.String ("E").Return (MapBlock.Cell.Label.E))
            .Or (Parse.String ("G").Return (MapBlock.Cell.Label.G))
            .Or (Parse.String ("").Return (MapBlock.Cell.Label.EMPTY))
            .Or (Parse.String ("GOAL").Return (MapBlock.Cell.Label.GOAL));

        static readonly Parser<IEnumerable<MapBlock.Cell.Label>> SymbolList =
            from leading in Symbol
            from rest in LabelSeparator.Then (_ => Symbol).Many ()
            select Cons (leading, rest);

        static readonly Parser<IEnumerable<MapBlock.Cell>> Row =
            from leadingLabelList in SymbolList.Token ()
            from restLabelList in CellSeparator.Then (_ => SymbolList.Token ()).Many ()
            let labelLists = Cons (leadingLabelList, restLabelList)
            let labels = labelLists.Select (ll => MapBlock.Cell.Label.Combined (ll))
            let cells = labels.Select(l => new MapBlock.Cell(l))
            select cells;

        static readonly Parser<IEnumerable<IEnumerable<MapBlock.Cell>>> Block =
            from leading in Row
            from rest in Parse.LineEnd.Then (_ => Row).Many ()
            from end in Parse.LineTerminator
            select Cons (leading, rest);

        static readonly Parser<IEnumerable<IEnumerable<MapBlock.Cell>>> BlockStatement =
            from blockStatementIdentifier in Parse.String ("block").Token ()
            from block in Block
            from post in Parse.LineEnd.Or(Parse.WhiteSpace.Many().Text()).Many()
            select block;

        public static readonly Parser<Map> StageStatement =
            from title in TitleStatement
            from type in MapTypeStatement
            from blocks in BlockStatement.Many ().End ()
            let mapBlocks = blocks.Select (b => new MapBlock (b))
            select new Map (title, type, mapBlocks);

        static IEnumerable<T> Cons<T>(T head, IEnumerable<T> rest)
        {
            yield return head;
            foreach ( var item in rest )
                yield return item;
        }
    }
}
