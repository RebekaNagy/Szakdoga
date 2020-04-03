using System;
using System.Collections.Generic;
using System.Text;
using P4Verification;

namespace P4Verification.Model
{
    class VerificationModel
    {
        public string InputString { get; set; }
        public string OutputString { get; set; }

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
