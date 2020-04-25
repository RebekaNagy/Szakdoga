using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Verification.Model
{
    class CalculationEventArgs : EventArgs
    {
        public string ResultFinalEnvs { get; private set; }

        public List<string> ResultInitEnvs { get; private set; }

        public List<IdEnvironment> ResultEnvs { get; private set; }

        public CalculationEventArgs(string finalenvs, List<string> initenvs, List<IdEnvironment> envs)
        {
            ResultFinalEnvs = finalenvs;
            ResultInitEnvs = initenvs;
            ResultEnvs = envs;
        }
    }
}
