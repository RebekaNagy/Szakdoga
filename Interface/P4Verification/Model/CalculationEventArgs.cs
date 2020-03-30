using System;
using System.Collections.Generic;
using System.Text;

namespace P4Verification.Model
{
    class CalculationEventArgs : EventArgs
    {
        public string Result { get; private set; }

        public CalculationEventArgs(string output)
        {
            Result = output;
        }
    }
}