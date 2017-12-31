using UnityEngine;
using System.Collections;

namespace Core.Map.Generator
{
    public interface MapGenerator
    {
        Map Generate();
    }
}
