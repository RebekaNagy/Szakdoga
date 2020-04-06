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
        private string _errorMessage;
        private bool _ruleMakerVisibility;
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

        public bool RuleMakerVisibility
        {
            get { return _ruleMakerVisibility; }
            set
            {
                if (_ruleMakerVisibility != value)
                {
                    _ruleMakerVisibility = value;
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
            RuleMakerVisibility = false;

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
            if(SelectedFirst == null || SelectedSecond == null || SelectedThird == null)
            {
                ErrorMessage = "Új szabály generálásához mind a három opció kiválasztására szükség van.";
            }
            else
            {
                ErrorMessage = "";
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
                        tempRule = tempRule + "assign ";
                        break;
                }
                tempRule = tempRule + "=>" + SelectedSecond + ", " + SelectedThird;
                NewStringRule = tempRule;
            }
        }

        private void AddRule()
        {
            if(NewStringRule == null || NewStringRule == "")
            {
                ErrorMessage = "Új hozzáadásához előbb a szabály generálására van szükség.";
            }
            else
            {
                ErrorMessage = "";
                StringRules.Add(NewStringRule);
                NewStringRule = "";

                Rules.Remove(SelectedFirst);
            }
        }

        private void DeleteRule()
        {
            if(SelectedStringRule.Contains("seq"))
            {
                Rules.Add(new SequenceRule("Sequence"));
            }
            else if(SelectedStringRule.Contains("select"))
            {
                Rules.Add(new SelectionRule("Selection"));
            }
            else if (SelectedStringRule.Contains("table"))
            {
                Rules.Add(new TableRule("Table"));
            }
            else if (SelectedStringRule.Contains("assign"))
            {
                Rules.Add(new AssignmentRule("Assignment"));
            }
            StringRules.Remove(SelectedStringRule);
        }
    }
}
