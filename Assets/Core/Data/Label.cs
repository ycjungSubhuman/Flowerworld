using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Newtonsoft.Json;

namespace Assets.Core.Data
{
    /* 각 칸의 색, 또는 역할을 나타내는 라벨 */
    public class Label
    {
        public Label() { } //dummy for deserialization
        private static Label genLabel(int value)
        {
            return new Label (value);
        }
        /*
         * 각 레이블 클래스 별로 int의 한 개의 비트를 차지하는 식으로 구현된다.
         * 따라서 레이블을 32개 종류 이상으로 만들 것이라면 구현 방식을 바꾸어야 한다. 
         * 
         * 새로운 레이블을 추가하고 싶다면 'genLabel(1 << (위에서 나타나지 않은 0~31 사이의 정수))'를 static readonly 필드로 추가해준다.
         */
        public static readonly Label EMPTY = genLabel (0);           // 절대 갈 수 없는 블럭, 또는 빈 공간을 나타내는 블럭
        public static readonly Label START = genLabel (1 << 0);
        public static readonly Label A = genLabel (1 << 1);
        public static readonly Label B = genLabel (1 << 2);
        public static readonly Label C = genLabel (1 << 3);
        public static readonly Label D = genLabel (1 << 4);
        public static readonly Label E = genLabel (1 << 5);
        public static readonly Label F = genLabel (1 << 6);
        public static readonly Label G = genLabel (1 << 7);
        public static readonly Label ANY = A.Or (B).Or (C).Or (D).Or (E).Or (F).Or (G);       // 아무 색이나 다 허용. 단, 알파벳 레이블만 허용한다.
        public static readonly Label GOAL = genLabel (1 << 30);

        public int Value { get { return value; } }

        [JsonProperty]
        private int value;

        private Label(int value)
        {
            this.value = value;
        }

        /**
         * 이 Label 객체가 주어진 'label'에 속하는지 검사한다. 
         *
         * eg)
         * Label.A.FallIn(Label.A); // =True
         * Label.WILDCARD.FallIn(Label.A); // =True
         */
        public bool FallIn(Label label)
        {
            return (label.value & this.value) != 0;
        }

        /**
         * 현재 Label 객체에 'label'과 합쳐진 속성을 지닌 Label을 리턴한다.  
         *
         * eg)
         * 시작점과 A라벨 속성을 동시에 지니도록 만들고 싶을 때
         *
         * var a = Label.START.Or(Label.A);
         * ...use a...
         */
        public Label Or(Label label)
        {
            return new Label (label.value | this.value);
        }

        /**
         * labels의 모든 레이블을 합친 Label을 리턴
         */
        public static Label Combined(IEnumerable<Label> labels)
        {
            return labels.Aggregate ((a, b) => a.Or (b));
        }
    }
}
