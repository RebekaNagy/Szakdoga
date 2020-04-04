using System;
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

        public List<String> Rules
        {
            get { return _rules; }
            private set
            {
                if (_rules != value)
                {
                    _rules = value;
                }
            }
        }
        public HaskellCalculation hscalculation { get; set; }

        public HaskellString hsstring { get; set; }

        public event EventHandler<CalculationEventArgs> CalculationDone;
        public VerificationModel() 
        {
            OutputString = "A végeredmény itt fog megjelenni.";
            InputString = "Kód bemásolása.";
            hscalculation = new HaskellCalculation();
            hsstring = new HaskellString();
        }

        public void Calculate(string input)
        {
            InputString = input;
//            OutputString = hsstring.CopyString(InputString);
            OutputString = hscalculation.HsCalculate(InputString);
            OnCalculationDone(OutputString);
        }

        private void OnCalculationDone(string n)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(n));
        }
    }
}
