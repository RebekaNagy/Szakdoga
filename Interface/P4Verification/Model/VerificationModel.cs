using System;
using System.Collections.Generic;
using System.Text;

namespace P4Verification.Model
{
    class VerificationModel
    {
        public string InputString { get; set; }
        public string OutputString { get; set; }

        public event EventHandler<CalculationEventArgs> CalculationDone;
        public VerificationModel() 
        {
            OutputString = "A végeredmény itt fog megjelenni.";
            InputString = "Kód bemásolása.";
        }

        public void Calculate(string input)
        {
            InputString = input;
            OutputString = InputString;
            OnCalculationDone(OutputString);
        }

        private void OnCalculationDone(string n)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(n));
        }
    }
}
