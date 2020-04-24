using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Verification.Model
{
    class VerificationModel
    {
        private string _inputString;
        private string _outputString;
        private string _errorString;
        private List<string> _environments;
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
        public string OutputString
        {
            get { return _outputString; }
            private set
            {
                if (_outputString != value)
                {
                    _outputString = value;
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

        public List<string> Environments
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
        public VerificationModel()
        {
            OutputString = "A végeredmény itt fog megjelenni.";
            InputString = "Kód bemásolása.";
            hscalculation = new HaskellCalculation();
            Environments = new List<string>();
        }

        public void Calculate(string input, string conds, bool locking)
        {
            if (locking)
            {
                if (input != null && input != "")
                {
                    InputString = input;
                    string tmp = hscalculation.HsCalculate(InputString, conds);
                    int index = tmp.IndexOf(':');
                    if (index > 0)
                    {
                        string errortmp = tmp.Substring(0, index);
                        if (errortmp == "NOERROR")
                        {
                            ErrorString = "";
                            string envs = tmp.Substring(index + 2);
                            index = envs.IndexOf("&");
                            string finalenvs = envs.Substring(index + 1);
                            envs = envs.Substring(0, index);
                            index = finalenvs.IndexOf("&");
                            string initenvs = finalenvs.Substring(index + 1);
                            finalenvs = finalenvs.Substring(0, index);
                            OutputString = "final:" + finalenvs + "\n\n" + "init:" + initenvs + "\n\n";

                            while(envs != string.Empty)
                            {
                                index = envs.IndexOf("#");
                                tmp = envs.Substring(0, index);
                                Environments.Add(tmp);
                                if(index + 1 < envs.Length)
                                {
                                    envs = envs.Substring(index + 1);
                                }
                                else
                                {
                                    envs = string.Empty;
                                }
                            }

                        }
                        else
                        {
                            ErrorString = errortmp;
                            OutputString = "";
                        }
                    }
                    else
                    {
                        ErrorString = "Ismeretlen hiba.";
                        OutputString = "";
                    }

                    OnCalculationDone(OutputString, Environments);
                    OnNewError(ErrorString);
                }
                else
                {
                    InputString = input;
                    OutputString = "";
                    OnCalculationDone(OutputString, Environments);
                    ErrorString = "Üres input.";
                    OnNewError(ErrorString);

                }
            }
            else
            {
                InputString = input;
                OutputString = "";
                OnCalculationDone(OutputString, Environments);
                ErrorString = "A kiértékeléshez szükséges a feltételek rögzítése.";
                OnNewError(ErrorString);
            }
        }

        private void OnCalculationDone(string output, List<string> envs)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(output, envs));
        }

        private void OnNewError(string n)
        {
            Error?.Invoke(this, new ErrorEventArgs(n));
        }
    }
}
