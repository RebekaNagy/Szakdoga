using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Verification.ViewModel
{
    public class Condition
    {
        public ObservableCollection<string> CondsCheck { get; set; }

        public Condition() 
        {
            CondsCheck = new ObservableCollection<string> { "Nincs", "Valid", "Invalid" };
        }
    }

    public class SelectCondition : Condition
    {
        public int CondsField { get; set; }
        public int CondsHeader { get; set; }
        public SelectCondition() : base()
        {
            CondsField = 0;
            CondsHeader = 0;
        }
    }

    public class TableCondition : Condition
    {
        public int KeysField { get; set; }
        public int KeysHeader { get; set; }
        public TableCondition() : base()
        {
            KeysField = 0;
            KeysHeader = 0;
        }
    }

    public class AssignmentCondition : Condition
    {
        public int LeftField { get; set; }
        public int LeftHeader { get; set; }
        public int RightField { get; set; }
        public int RightHeader { get; set; }
        public AssignmentCondition() : base()
        {
            LeftField = 0;
            LeftHeader = 0;
            RightField = 0;
            RightHeader = 0;
        }
    }

    public class SetHeaderCondition : Condition
    {
        public int Fields { get; set; }
        public int Header { get; set; }
        public SetHeaderCondition() : base()
        {
            Fields = 0;
            Header = 0;
        }
    }

    public class DropCondition : Condition
    {
        public int DropValidity { get; set; }
        public int Fields { get; set; }
        public int Headers { get; set; }
        public DropCondition() : base()
        {
            DropValidity = 0;
            Fields = 0;
            Headers = 0;            
        }
    }
}
