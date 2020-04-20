using P4Verification.Model;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;

namespace P4Verification.ViewModel
{
    class VerificationViewModel : ViewModelBase
    {
        private VerificationModel Model;
        private string _errorMessage;
        private int _selectedSelect;
        private int _selectedTable;
        private int _selectedAssignment;
        private int _selectedSetHeader;
        private int _selectedDrop;
        private bool _locking;
        private bool _editing;
        public int SelectedSelect
        {
            get { return _selectedSelect; }
            set
            {
                if (_selectedSelect != value)
                {
                    _selectedSelect = value;
                    OnPropertyChanged();
                }
            }
        }

        public int SelectedTable
        {
            get { return _selectedTable; }
            set
            {
                if (_selectedTable != value)
                {
                    _selectedTable = value;
                    OnPropertyChanged();
                }
            }
        }
        public int SelectedAssignment
        {
            get { return _selectedAssignment; }
            set
            {
                if (_selectedAssignment != value)
                {
                    _selectedAssignment = value;
                    OnPropertyChanged();
                }
            }
        }
        public int SelectedSetHeader
        {
            get { return _selectedSetHeader; }
            set
            {
                if (_selectedSetHeader != value)
                {
                    _selectedSetHeader = value;
                    OnPropertyChanged();
                }
            }
        }
        public int SelectedDrop
        {
            get { return _selectedDrop; }
            set
            {
                if (_selectedDrop != value)
                {
                    _selectedDrop = value;
                    OnPropertyChanged();
                }
            }
        }

        public string ErrorMessage
        {
            get { return _errorMessage; }
            set
            {
                if (_errorMessage != value)
                {
                    _errorMessage = value;
                    OnPropertyChanged();
                }
            }
        }

        public bool Locking
        {
            get { return _locking; }
            set
            {
                if (_locking != value)
                {
                    _locking = value;
                    OnPropertyChanged();
                }
            }
        }

        public bool Editing
        {
            get { return _editing; }
            set
            {
                if (_editing != value)
                {
                    _editing = value;
                    OnPropertyChanged();
                }
            }
        }

        public string ConditionString { get; set; }

        public SelectCondition SelectConds { get; set; }
        public TableCondition TableConds { get; set; }
        public AssignmentCondition AssignmentConds { get; set; }
        public SetHeaderCondition SetHeaderConds { get; set; }
        public DropCondition DropConds { get; set; }
        public string Output
        {
            get; set;
        }
        public string Input
        {
            get; set;
        }

        public event EventHandler ReadInput;

        public DelegateCommand CalculateCommand { get; set; }
        public DelegateCommand ReadInputCommand { get; set; }
        public DelegateCommand LockCommand { get; set; }
        public DelegateCommand EditCommand { get; set; }
        public VerificationViewModel(VerificationModel model)
        {
            Model = model;

            SelectConds = new SelectCondition();
            TableConds = new TableCondition();
            AssignmentConds = new AssignmentCondition();
            SetHeaderConds = new SetHeaderCondition();
            DropConds = new DropCondition();

            SelectedSelect = 0;
            SelectedTable = 0;
            SelectedAssignment = 0;
            SelectedDrop = 0;
            SelectedSetHeader = 0;
            SelectedDrop = 0;

            Locking = false;
            Editing = true;

            model.CalculationDone += new EventHandler<CalculationEventArgs>(Model_CalculationDone);
            model.Error += new EventHandler<ErrorEventArgs>(Model_Error);

            CalculateCommand = new DelegateCommand(param => Model.Calculate(Input, ConditionString, Locking));
            ReadInputCommand = new DelegateCommand(param => OnReadInput());
            LockCommand = new DelegateCommand(param => LockConditions());
            EditCommand = new DelegateCommand(param => EditConditions());
        }
        private void Model_CalculationDone(object sender, CalculationEventArgs e)
        {
            this.Output = e.Result;
            OnPropertyChanged("Output");
        }

        private void Model_Error(object sender, ErrorEventArgs e)
        {
            this.ErrorMessage = e.ErrorString;
        }
        private void OnReadInput()
        {
            ReadInput?.Invoke(this, EventArgs.Empty);
        }

        private void LockConditions()
        {
            ConditionString = SelectedSelect.ToString() +
                SelectedTable.ToString() +
                SelectedAssignment.ToString() +
                SelectedSetHeader.ToString() +
                SelectedDrop.ToString();
            Locking = true;
            Editing = false;

        }
        private void EditConditions()
        {
            Locking = false;
            Editing = true;
        }
    }
}
