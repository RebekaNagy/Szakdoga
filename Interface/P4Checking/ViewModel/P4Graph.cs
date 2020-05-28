using GraphSharp.Controls;
using QuickGraph;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;

namespace P4Checking.ViewModel
{
    public class P4Vertex
    {
        public string Id { get; set; }
        public string Name { get; set; }        
        public Brush VertexColor { get; set; }

        public P4Vertex(string id, string name)
        {
            Id = id;
            Name = name;
        }
    }
    public class P4Graph : BidirectionalGraph<P4Vertex, IEdge<P4Vertex>> { }
    
    public class P4GraphLayout : GraphLayout<P4Vertex, IEdge<P4Vertex>, P4Graph> { }

}