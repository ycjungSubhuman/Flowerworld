using Assets.Core.Data;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

namespace Assets.Core.Drawer
{
    class CellColor
    {
        public static Dictionary<int, Color> lineColorMap =
            new Dictionary<int, Color> ()
            {
                {Label.A.Value, rgb(196, 0, 103)},
                {Label.B.Value, rgb(144, 224, 0)},
                {Label.C.Value, rgb(241, 129, 0)},
                {Label.D.Value, rgb(3, 103, 152)},
                {Label.ANY.Value, rgb(33, 33, 33)},
                {Label.EMPTY.Value, rgba(255, 255, 255, 0)},
            };

        public static Dictionary<int, Color> bgColorMap =
            new Dictionary<int, Color> ()
            {
                {Label.A.Value, rgb(240, 0, 127)},
                {Label.B.Value, rgb(160, 249, 0)},
                {Label.C.Value, rgb(255, 137, 0)},
                {Label.D.Value, rgb(8, 154, 225)},
                {Label.ANY.Value, rgb(0, 0, 0)},
                {Label.EMPTY.Value, rgba(200, 200, 200, 0)},
            };

        public static Color rgb(int r, int g, int b)
        {
            return new Color (r / 255.0f, g / 255.0f, b / 255.0f);
        }

        public static Color rgba(int r, int g, int b, int a)
        {
            return new Color (r / 255.0f, g / 255.0f, b / 255.0f, a / 255.0f);
        }

        public static Color getLineColorOf (Label label)
        {
            return lineColorMap [label.Value 
                & (~Label.START.Value)
                & (~Label.GOAL.Value) ];
        }

        public static Color getBackgroundColorOf (Label label)
        {
            return bgColorMap [label.Value 
                & (~Label.START.Value)
                & (~Label.GOAL.Value) ];
        }
    }
}
