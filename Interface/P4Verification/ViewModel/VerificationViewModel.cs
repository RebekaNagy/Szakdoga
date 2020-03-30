using P4Verification.Model;
using System;
using System.Collections.Generic;
using System.Text;

namespace P4Verification.ViewModel
{
    class VerificationViewModel : ViewModelBase
    {
        private VerificationModel Model;

        public string Output
        {
            get; set;
        }
        public string Input
        {
            get; set;
        }

        public event EventHandler ReadInput;

        public DelegateCommand CalculateCommand { get; set; }
        public DelegateCommand ReadInputCommand { get; set; }

        public VerificationViewModel(VerificationModel model)
        {
            Model = model;

            model.CalculationDone += new EventHandler<CalculationEventArgs>(Model_CalculationDone);

            CalculateCommand = new DelegateCommand(param => Model.Calculate(Input));
            ReadInputCommand = new DelegateCommand(param => OnReadInput());

        }
        private void Model_CalculationDone(object sender, CalculationEventArgs e)
        {
            this.Output = e.Result;
            OnPropertyChanged("Output");
        }
        private void OnReadInput()
        {
            ReadInput?.Invoke(this, EventArgs.Empty);
        }
    }
}
