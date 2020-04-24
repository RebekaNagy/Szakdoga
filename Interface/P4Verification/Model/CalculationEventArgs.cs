using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Verification.Model
{
    class CalculationEventArgs : EventArgs
    {
        public string ResultOutput { get; private set; }

        public List<string> ResultEnvs { get; private set; }

        public CalculationEventArgs(string output, List<string> envs)
        {
            ResultOutput = output;
            ResultEnvs = envs;

        }
    }
}
