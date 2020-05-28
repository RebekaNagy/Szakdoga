using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Checking.Model
{
    class CheckingModel
    {
        private string _inputString;
        private string _errorString;
        private List<IdEnvironment> _environments;
        private List<IdEnvironment> _finalenvs;
        private List<IdEnvironment> _initenvs;
        public string InputString
        {
            get { return _inputString; }
            private set
            {
                if (_inputString != value)
                {
                    _inputString = value;
                }
            }
        }

        public List<IdEnvironment> FinalEnvs
        {
            get { return _finalenvs; }
            private set
            {
                if (_finalenvs != value)
                {
                    _finalenvs = value;
                }
            }
        }

        public List<IdEnvironment> InitEnvs
        {
            get { return _initenvs; }
            private set
            {
                if (_initenvs != value)
                {
                    _initenvs = value;
                }
            }
        }

        public string ErrorString
        {
            get { return _errorString; }
            private set
            {
                if (_errorString != value)
                {
                    _errorString = value;
                }
            }
        }

        public List<IdEnvironment> Environments
        {
            get { return _environments; }
            private set
            {
                if (_environments != value)
                {
                    _environments = value;
                }
            }
        }

        public HaskellCalculation hscalculation { get; set; }


        public event EventHandler<CalculationEventArgs> CalculationDone;
        public event EventHandler<ErrorEventArgs> Error;
        public CheckingModel()
        {
            InputString = "Kód bemásolása.";
            hscalculation = new HaskellCalculation();
            Environments = new List<IdEnvironment>();
            FinalEnvs = new List<IdEnvironment>();
            InitEnvs = new List<IdEnvironment>();
        }

        public void Calculate(string input, string conds)
        {
            FinalEnvs.Clear();
            InitEnvs.Clear();
            Environments.Clear();
            ErrorString = "";
            InputString = input;

            if (InputString != null && InputString != "")
            {
                string tmp = hscalculation.HsCalculate(InputString, conds);

                var parts = tmp.Split('&');

                if (parts.Length == 4)
                {
                    if (parts[0] == "NOERROR")
                    {
                        processEnvs(parts[1]);
                        processFinalEnvs(parts[2]);
                        processInitEnvs(parts[3]);
                    }
                }
                else if(parts.Length == 2)
                {
                    ErrorString = parts[0];
                }
                else
                {
                    ErrorString = "Ismeretlen hiba.";
                }
            }
            else
            {
                ErrorString = "Üres input.";
            }
            OnCalculationDone(FinalEnvs, InitEnvs, Environments);
            OnNewError(ErrorString);
        }

        private void OnCalculationDone(List<IdEnvironment> finalenvs, List<IdEnvironment> initenvs, List<IdEnvironment> envs)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(finalenvs, initenvs, envs));
        }

        private void OnNewError(string n)
        {
            Error?.Invoke(this, new ErrorEventArgs(n));
        }

        private void processEnvs(string envs)
        {
            var tmpenvs = envs.Split('#');

            foreach (var tmpenv in tmpenvs)
            {
                if(tmpenv != "" || tmpenv != string.Empty)
                {
                    var envparts = tmpenv.Split('@');
                    var idparts = envparts[0].Split('$').ToList();
                    Environments.Add(new IdEnvironment
                    {
                        LeafEnv = envparts[2],
                        EnvType =  envparts[1],
                        EnvId = idparts
                    });
                }
            }

        }

        private void processInitEnvs(string envs)
        {
            var tmpenvs = envs.Split('#');

            foreach (var tmpenv in tmpenvs)
            {
                if (tmpenv != "" || tmpenv != string.Empty)
                {
                    var parts = tmpenv.Split('@');
                    InitEnvs.Add(new IdEnvironment
                    {
                        EnvId = new List<string> {parts[0]},
                        EnvType = "",
                        LeafEnv = parts[1]
                    });
                }
            }
        }

        private void processFinalEnvs(string envs)
        {
            var tmpenvs = envs.Split('#');

            foreach (var tmpenv in tmpenvs)
            {
                if (tmpenv != "" || tmpenv != string.Empty)
                {
                    FinalEnvs.Add(new IdEnvironment
                    {
                        EnvId = new List<string>(),
                        EnvType = "",
                        LeafEnv = tmpenv
                    });
                }
            }
        }
    }
}
