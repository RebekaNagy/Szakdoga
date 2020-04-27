using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using GraphSharp.Controls;
using P4Verification.Model;
using QuickGraph;
using GraphSharp;
using static P4Verification.ViewModel.P4Graph;
using System.Collections.ObjectModel;

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
        private List<IdEnvironment> _calculatedEnvironments;
        private ObservableCollection<IdEnvironment> _initEnvironments;
        private IdEnvironment _selectedInitEnv;
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

        private P4Graph _graphToVisualize;

        public P4Graph GraphToVisualize
        {
            get { return _graphToVisualize; }
            set
            {
                if (_graphToVisualize != value)
                {
                    _graphToVisualize = value;
                    OnPropertyChanged();
                }
            }
        }

        public List<IdEnvironment> CalculatedEnvironments
        {
            get { return _calculatedEnvironments; }
            set
            {
                if (_calculatedEnvironments != value)
                {
                    _calculatedEnvironments = value;
                    OnPropertyChanged();
                }
            }
        }

        public ObservableCollection<IdEnvironment> InitEnvironments
        {
            get { return _initEnvironments; }
            set
            {
                if (_initEnvironments != value)
                {
                    _initEnvironments = value;
                    OnPropertyChanged();
                }
            }
        }

        public IdEnvironment SelectedInitEnv
        {
            get { return _selectedInitEnv; }
            set
            {
                if (_selectedInitEnv != value)
                {
                    _selectedInitEnv = value;
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

        public DelegateCommand MakeGraphCommand { get; set; }
        public VerificationViewModel(VerificationModel model)
        {
            Model = model;
            
            SelectConds = new SelectCondition();
            TableConds = new TableCondition();
            AssignmentConds = new AssignmentCondition();
            SetHeaderConds = new SetHeaderCondition();
            DropConds = new DropCondition();

            GraphToVisualize = new P4Graph();

            CalculatedEnvironments = new List<IdEnvironment>();
            InitEnvironments = new ObservableCollection<IdEnvironment>();
            SelectedInitEnv = new IdEnvironment();

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
            MakeGraphCommand = new DelegateCommand(param => MakeGraph());
        }
        private void Model_CalculationDone(object sender, CalculationEventArgs e)
        {
            this.Output = e.ResultFinalEnvs;
            OnPropertyChanged("Output");
            CalculatedEnvironments = new List<IdEnvironment>();
            this.CalculatedEnvironments = e.ResultEnvs;
            OnPropertyChanged("CalculatedEnvironments");
            this.InitEnvironments = new ObservableCollection<IdEnvironment>(e.ResultInitEnvs);
            OnPropertyChanged("InitEnvironments");
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

        private void MakeGraph()
        {
            if(SelectedInitEnv.EnvId != null)
            {
                ErrorMessage = "";
                CreateGraphToVisualize(SelectedInitEnv.EnvId[0]);
            }
            else
            {
                ErrorMessage = "Válasszon ki egy kezdőkörnyezetet!";
                OnPropertyChanged("ErrorMessage");
            }
        }

        private void CreateGraphToVisualize(string selectedEnvId)
        {            
            var g = new P4Graph();
            GraphToVisualize.Clear();
            
            foreach (var env in CalculatedEnvironments)
            {
                if(env.EnvId[0] == selectedEnvId)
                {
                    var count = env.EnvId.Count();
                    List<P4Vertex> vertices = new List<P4Vertex>();

                    for (int i = 0; i < count; i++)
                    {
                        vertices.Add(new P4Vertex(env.EnvId[i]));
                        vertices[i].VertexColor = new SolidColorBrush(Colors.White);
                        P4Vertex result = g.Vertices.Where(x => x.Name == vertices[i].Name).FirstOrDefault();
                        if (result == null)
                        {
                            g.AddVertex(vertices[i]);
                        }
                        else
                        {
                            vertices[i] = result;
                        }
                        if (i != 0)
                        {
                            g.AddEdge(new Edge<P4Vertex>(vertices[i - 1], vertices[i]));
                        }
                    }

                    SolidColorBrush leafcolor = new SolidColorBrush();
                    switch (env.EnvType)
                    {
                        case "NoMatch":
                            leafcolor = new SolidColorBrush(Colors.Yellow);
                            break;
                        case "Match":
                            leafcolor = new SolidColorBrush(Colors.Green);
                            break;
                        case "Stuck":
                            leafcolor = new SolidColorBrush(Colors.Red);
                            break;
                    }
                    vertices.Add(new P4Vertex(env.LeafEnv) { VertexColor = leafcolor });
                    g.AddVertex(vertices[count]);
                    g.AddEdge(new Edge<P4Vertex>(vertices[count - 1], vertices[count]));
                }                  

            }
            GraphToVisualize = g;          

        }
    }
    
}
