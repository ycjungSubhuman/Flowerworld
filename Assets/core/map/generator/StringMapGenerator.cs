using UnityEngine;
using System.Collections;
using Sprache;

namespace Core.Map.Generator
{
    /** 
     * 맵 제작을 위한 간단한 텍스트 기반 맵 생성기.
     * 
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
     * 'cell'               ::= 'empty' | 'cell','symbol'
     * 'row'                ::= 'empty' | 'row'('whitespace'*)'cell'('whitespace'*)"|"
     * 'block_statement'    ::= block('whitespace'*)('newline'+)('row''whitespace'*'newline')+(('newline' | 'whitespace)*)
     * 'map_statement'      ::= 'block_statement'+ 
     * 
     * eg)
     * 
     * block
     * 
     *     START,ANY |  B   |   A   |   B   |   A      |
     *         B    |  C    |   B   |   A   |   A        |
     *         A    |  C    |       |       |           |
     *              |  A    |   B   |   C   |            |
     *              |       |   A   |   B   |   C,GOAL   |
     *              
     * block
     * 
     *     ...
     * 
     * 유의사항)
     * 1. 블럭이 여러 개 정의될 경우, 가장 먼저 정의된 block이 메인 맵으로 정해진다. 나머지는 드래그 가능한 서브 맵으로 할당된다.
     * 2. 빈 row는 모두 EMPTY인 한 row로 여겨진다.
     * 3. 각 row의 길이는 똑같지 않아도 된다.
     */
    public class StringMapGenerator : MapGenerator
    {
        private string source;

        public StringMapGenerator(string source)
        {
            this.source = source;
        }

        public Map Generate()
        {
            throw new System.NotImplementedException ();
        }
    }
}
