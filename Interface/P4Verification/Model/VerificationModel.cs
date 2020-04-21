﻿using System;
using System.Collections.Generic;
using System.Text;
using P4Verification;

namespace P4Verification.Model
{
    class VerificationModel
    {
        private string _inputString;
        private string _outputString;
        private List<String> _rules;
        private string _errorString;
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
        
        public HaskellCalculation hscalculation { get; set; }


        public event EventHandler<CalculationEventArgs> CalculationDone;
        public event EventHandler<ErrorEventArgs> Error;
        public VerificationModel() 
        {
            OutputString = "A végeredmény itt fog megjelenni.";
            InputString = "Kód bemásolása.";
            hscalculation = new HaskellCalculation();
        }

        public void Calculate(string input, string conds, bool locking)
        {
            if(locking)
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
                            OutputString = tmp.Substring(index);
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

                    OnCalculationDone(OutputString);
                    OnNewError(ErrorString);
                }
                else
                {
                    InputString = input;
                    OutputString = "";
                    OnCalculationDone(OutputString);
                    ErrorString = "Üres input.";
                    OnNewError(ErrorString);

                }
            }
            else
            {
                InputString = input;
                OutputString = "";
                OnCalculationDone(OutputString);
                ErrorString = "A kiértékeléshez szükséges a feltételek rögzítése.";
                OnNewError(ErrorString);
            }
        }

        private void OnCalculationDone(string n)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(n));
        }

        private void OnNewError(string n)
        {
            Error?.Invoke(this, new ErrorEventArgs(n));
        }
    }
}
