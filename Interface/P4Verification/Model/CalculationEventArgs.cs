using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace P4Verification.Model
{
    class CalculationEventArgs : EventArgs
    {
        public List<IdEnvironment> ResultFinalEnvs { get; private set; }

        public List<IdEnvironment> ResultInitEnvs { get; private set; }

        public List<IdEnvironment> ResultEnvs { get; private set; }

        public CalculationEventArgs(List<IdEnvironment> finalenvs, List<IdEnvironment> initenvs, List<IdEnvironment> envs)
        {
            ResultFinalEnvs = finalenvs;
            ResultInitEnvs = initenvs;
            ResultEnvs = envs;
        }
    }
}
