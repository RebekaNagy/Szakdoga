using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Checking.ViewModel
{
    public class Condition
    {
        public ObservableCollection<string> CondsCheck { get; set; }

        public Condition() 
        {
            CondsCheck = new ObservableCollection<string> { "Nincs", "Valid", "Invalid" };
        }

        public event EventHandler<ConditionEventArgs> ConditionChanged;

        public void OnConditionChanged()
        {
            ConditionChanged?.Invoke(this, new ConditionEventArgs());
        }
    }

    public class SelectCondition : Condition
    {
        private int _condsField;
        private int _condsHeader;
        public int CondsField 
        {
            get
            {
                return _condsField;
            }
            set 
            { 
                if (_condsField != value)
                {
                    _condsField = value;
                    OnConditionChanged();
                }
            } 
        }
        public int CondsHeader
        {
            get
            {
                return _condsHeader;
            }
            set
            {
                if (_condsHeader != value)
                {
                    _condsHeader = value;
                    OnConditionChanged();
                }
            }
        }
        public SelectCondition() : base()
        {
            CondsField = 0;
            CondsHeader = 0;
        }
    }

    public class TableCondition : Condition
    {
        private int _keysField;
        private int _keysHeader;
        public int KeysField
        {
            get
            {
                return _keysField;
            }
            set
            {
                if (_keysField != value)
                {
                    _keysField = value;
                    OnConditionChanged();
                }
            }
        }
        public int KeysHeader
        {
            get
            {
                return _keysHeader;
            }
            set
            {
                if (_keysHeader != value)
                {
                    _keysHeader = value;
                    OnConditionChanged();
                }
            }
        }
        public TableCondition() : base()
        {
            KeysField = 0;
            KeysHeader = 0;
        }
    }

    public class AssignmentCondition : Condition
    {
        private int _leftField;
        private int _leftHeader;
        private int _rightField;
        private int _rightHeader;
        public int LeftField
        {
            get
            {
                return _leftField;
            }
            set
            {
                if (_leftField != value)
                {
                    _leftField = value;
                    OnConditionChanged();
                }
            }
        }
        public int LeftHeader
        {
            get
            {
                return _leftHeader;
            }
            set
            {
                if (_leftHeader != value)
                {
                    _leftHeader = value;
                    OnConditionChanged();
                }
            }
        }
        public int RightField
        {
            get
            {
                return _rightField;
            }
            set
            {
                if (_rightField != value)
                {
                    _rightField = value;
                    OnConditionChanged();
                }
            }
        }
        public int RightHeader
        {
            get
            {
                return _rightHeader;
            }
            set
            {
                if (_rightHeader != value)
                {
                    _rightHeader = value;
                    OnConditionChanged();
                }
            }
        }
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
        private int _fields;
        private int _header;
        public int Fields
        {
            get
            {
                return _fields;
            }
            set
            {
                if (_fields != value)
                {
                    _fields = value;
                    OnConditionChanged();
                }
            }
        }
        public int Header
        {
            get
            {
                return _header;
            }
            set
            {
                if (_header != value)
                {
                    _header = value;
                    OnConditionChanged();
                }
            }
        }
        public SetHeaderCondition() : base()
        {
            Fields = 0;
            Header = 0;
        }
    }

    public class DropCondition : Condition
    {
        private int _dropValidity;
        private int _fields;
        private int _headers;
        public int DropValidity
        {
            get
            {
                return _dropValidity;
            }
            set
            {
                if (_dropValidity != value)
                {
                    _dropValidity = value;
                    OnConditionChanged();
                }
            }
        }
        public int Fields
        {
            get
            {
                return _fields;
            }
            set
            {
                if (_fields != value)
                {
                    _fields = value;
                    OnConditionChanged();
                }
            }
        }
        public int Headers
        {
            get
            {
                return _headers;
            }
            set
            {
                if (_headers != value)
                {
                    _headers = value;
                    OnConditionChanged();
                }
            }
        }
        public DropCondition() : base()
        {
            DropValidity = 0;
            Fields = 0;
            Headers = 0;            
        }
    }

    public class ConditionEventArgs : EventArgs
    {
        public ConditionEventArgs() { }
    }
}
