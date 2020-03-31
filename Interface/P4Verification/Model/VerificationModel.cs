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

        public Fibonacci fi { get; set; }

        public event EventHandler<CalculationEventArgs> CalculationDone;
        public VerificationModel() 
        {
            OutputString = "A végeredmény itt fog megjelenni.";
            InputString = "Kód bemásolása.";
            fi = new Fibonacci();
        }

        public void Calculate(string input)
        {
            InputString = input;
            OutputString = InputString;
            var number = fi.fibonacci(8);
            OnCalculationDone(OutputString + number.ToString());
        }

        private void OnCalculationDone(string n)
        {
            CalculationDone?.Invoke(this, new CalculationEventArgs(n));
        }
    }
}
