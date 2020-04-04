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
        private ObservableCollection<String> _stringRules;
        private string _selectedStringRule;
        private Rule _selectedFirst;
        private string _selectedSecond;
        private string _selectedThird;
        private string _newStringRule;

        public Rule SelectedFirst
        {
            get { return _selectedFirst; }
            set
            {
                if (_selectedFirst != value)
                {
                    _selectedFirst = value;
                    OnPropertyChanged();
                }
            }
        }
        public string SelectedSecond
        {
            get { return _selectedSecond; }
            set
            {
                if (_selectedSecond != value)
                {
                    _selectedSecond = value;
                    OnPropertyChanged();
                }
            }
        }
        public string SelectedThird
        {
            get { return _selectedThird; }
            set
            {
                if (_selectedThird != value)
                {
                    _selectedThird = value;
                    OnPropertyChanged();
                }
            }
        }
        public ObservableCollection<Rule> Rules { get; set; }
        public ObservableCollection<string> StringRules
        {
            get { return _stringRules; }
            set
            {
                if (_stringRules != value)
                {
                    _stringRules = value;
                    OnPropertyChanged();
                }
            }
        }

        public string SelectedStringRule
        {
            get { return _selectedStringRule; }
            set
            {
                if(_selectedStringRule != value)
                {
                    _selectedStringRule = value;
                    OnPropertyChanged();
                }
            }
        }

        public string NewStringRule
        {
            get { return _newStringRule; }
            set
            {
                if(_newStringRule != value)
                {
                    _newStringRule = value;
                    OnPropertyChanged();
                }
            }
        }

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
        public DelegateCommand GenerateRuleCommand { get; set; }
        public DelegateCommand AddRuleCommand { get; set; }

        public DelegateCommand DeleteRuleCommand { get; set; }
        public VerificationViewModel(VerificationModel model)
        {
            Model = model;

            Rules = new ObservableCollection<Rule>(new List<Rule>
            {
                new SequenceRule("Sequence"),
                new SelectionRule("Selection"),
                new TableRule("Table"),
                new AssignmentRule("Assignment")
            });

            StringRules = new ObservableCollection<string>();

            model.CalculationDone += new EventHandler<CalculationEventArgs>(Model_CalculationDone);

            CalculateCommand = new DelegateCommand(param => Model.Calculate(Input));
            ReadInputCommand = new DelegateCommand(param => OnReadInput());
            GenerateRuleCommand = new DelegateCommand(param => GenerateRule());
            AddRuleCommand = new DelegateCommand(param => AddRule());
            DeleteRuleCommand = new DelegateCommand(param => DeleteRule());

        }
        private void Model_CalculationDone(object sender, CalculationEventArgs e)
        {
            this.Output = e.Result;
            OnPropertyChanged("Output");
        }
        private void OnReadInput()
        {
            ReadInput?.Invoke(this, EventArgs.Empty);
        }

        private void GenerateRule()
        {
            string tempRule = "";
            switch (SelectedFirst.Name)
            {
                case "Sequence":
                    tempRule = tempRule + "seq ";
                    break;
                case "Selection":
                    tempRule = tempRule + "select ";
                    break;
                case "Table":
                    tempRule = tempRule + "table ";
                    break;
                case "Assignment":
                    tempRule = tempRule + "assignt ";
                    break;
            }
            tempRule = tempRule + "=>" + SelectedSecond + ", " + SelectedThird;
            NewStringRule = tempRule;
        }

        private void AddRule()
        {
            StringRules.Add(NewStringRule);
            NewStringRule = "";
        }

        private void DeleteRule()
        {
            StringRules.Remove(SelectedStringRule);
        }
    }
}
