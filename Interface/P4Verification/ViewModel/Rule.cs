﻿using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;

namespace P4Verification.ViewModel
{
    /*
    E : h.setinvalid() => E[h->invalid], True

    E: S1;S2   => 
    (E: S1) : S2, True

    E: if (b) P1 => [E[C ++ b]: P1] ++ [E[C ++ -b]: Skip], b.headers_fields().Valid()

    E: table k: {ki} a : {ai} =>  E: ai   , ki.headers_fields().Valid()

    | Seq 
    | If 
    | Table 
    | ActCons 
    | ActAssignment [String]
    | Drop 
    | SetHeaderValidity
    */
    public class Rule
    {
        public string Name { get; set; }

        public ObservableCollection<string> RuleActions {get; set;}
        public ObservableCollection<string> SideConditions { get; set; }
        public Rule(string name)
        {
            Name = name;
        }

    }

    public class SequenceRule : Rule
    {
        public SequenceRule(string name) : base(name)
        {
            RuleActions = new ObservableCollection<string>(new List<string> { "seqvalid", "seqinvalid" });
            SideConditions = new ObservableCollection<string>(new List<string> { "seqTrue" });
        }
    }

    public class SelectionRule : Rule
    {
        public SelectionRule(string name) : base(name)
        {
            RuleActions = new ObservableCollection<string>(new List<string> { "selectvalid", "selectinvalid" });
            SideConditions = new ObservableCollection<string>(new List<string> { "selectTrue" });
        }
    }

    public class TableRule : Rule
    {
        public TableRule(string name) : base(name)
        {
            RuleActions = new ObservableCollection<string>(new List<string> { "tablevalid", "tableinvalid" });
            SideConditions = new ObservableCollection<string>(new List<string> { "tableTrue" });
        }
    }

    public class AssignmentRule : Rule
    {
        public AssignmentRule(string name) : base(name)
        {
            RuleActions = new ObservableCollection<string>(new List<string> { "assignvalid", "assigninvalid" });
            SideConditions = new ObservableCollection<string>(new List<string> { "assignTrue" });
        }
    }


}