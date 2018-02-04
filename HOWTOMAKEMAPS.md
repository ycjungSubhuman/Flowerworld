# 맵 만드는 방법

## .map파일 컴파일 하는 방법

.map 문법에 맞게 맵 파일을 작성하시면 게임에서 사용할 수 있는 맵 데이터로 컴파일할 수 있습니다. 

1. Assets/maps/ 아래에 .map 확장자를 가진 텍스트 파일들을 만든다.
1. compileAll.bat을 실행시킨다. 커맨드 창이 뜬다.
1. 커맨드 창을 통해 맵들의 컴파일 결과를 알 수 있다. 컴파일 실패한 맵은 실패 이유를 확인할 수 있다. 
1. Assets/Resources/maps/ 아래에 .json 으로 컴파일 되는 것을 확인할 수 있다. (이 파일들을 게임에서 읽어들입니다.) 

## 자동 컴파일 하는 방법 

매번 compileAll.bat을 실행시키는 것이 번거로울 수도 있습니다. 그래서 파일 추가/수정/삭제시마다 자동으로 컴파일해주는
기능을 준비했습니다. .map파일 추가/수정 시 그 순간 자동으로 .json으로 컴파일하고, .map 삭제시에는 대응되는 .json을 삭제해줍니다.

1. watch.bat을 실행시킨다. 커맨드 창이 뜬다.
1. 우선 Assets/maps/ 의 .map들을 전부 Assets/Resources/maps/ 안의 .json으로 컴파일해준다.
1. 'Started Watching ...'이 출력된 후로는 변경사항이 자동으로 컴파일된다.

## .map 문법

*는 ANY,
^는 START,
$는 GOAL을 의미합니다.


```
title (타이틀)
maptype (NORMAL | CONSTRUCT | DODGE)
pattern (","로 구별된 레이블)
block
    ("줄바꿈"으로 구별된 ("|"로 구별된 (","로 구별된 레이블)))

block
    ("줄바꿈"으로 구별된 ("|"로 구별된 (","로 구별된 레이블)))

block
    ("줄바꿈"으로 구별된 ("|"로 구별된 (","로 구별된 레이블)))

...

((여기에서 레이블은 A | B | C | D | E | F | G | ^ | $ | * 중 하나))
```

빠르게 작성을 시작하고 싶다면 밑 문단의 예제를 살펴보시기 바랍니다.

### 만족해야 하는 규칙

변환기가 알아서 어긴 규칙을 알려주겠지만, 참고 사항으로 적습니다. 

1. title, maptype, pattern을 딱 하나씩 반드시 가져야 한다.
1. maptype이 NORMAL 또는 DODGE일 경우, 딱 하나의 block만을 가져야 한다. 
1. maptype이 CONSTRUCT일 경우, 하나 이상의 block을 가져야 한다. 이 때 맨 첫번째 block이 메인 맵이 된다. 나머지는 드래그 가능한 맵 조각이 된다.
1. maptype이 NORMAL 또는 CONSTRUCT일 경우 전 block을 통틀어 반드시 하나의 ^와 하나의 $을 가져야 한다. 
1. maptype이 DODGE일 경우 하나의 ^와 0개의 $을 가져야 한다. 
1. pattern은 A | B | C | D | E | F | G 로만 이루어진다. 길이는 1 이상이어야 한다. 

### 예제

```
title 테스트맵 1
type NORMAL
pattern A,B,C,D
block
    A,^|   B   |   C   |   D   |
           |   A   |
          |  A,$ | 
```

```
title 테스트맵 2
type CONSTRUCT
pattern A,B,C,D
block
    A,^|   B   |   C   |   D   |
           |   A   |
          |  A,$ | 

block
   A  |   B   | 
   A  |   C   |

block
   C  |   C   | 
   C  |   C   |
```

```
title 테스트맵 3
type DODGE
pattern A,B,C,D
block
    A,^|   B   |   C   |   D   |
      C    |   A   |   C   |   D   |
      A    |  A    |   B   |   A   |
```



