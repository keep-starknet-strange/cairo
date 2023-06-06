use std::collections::VecDeque;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

use crate::items::imp::{ImplId, ImplLookupContext, UninferredImpl};
use crate::{ConcreteTraitId, GenericArgumentId, GenericParam};

#[derive(Copy, Clone)]
pub struct Context;

/// Canonical concrete trait.
#[derive(Copy, Clone)]
pub struct Goal {
    concrete_trait: ConcreteTraitId,
    // n_vars?
}
impl Goal {
    fn find_candidates(&self, ctx: Context, _scope: &Scope) -> Vec<Candidate> {
        todo!()
    }

    fn fetch_solution_set(&self, ctx: Context) -> SolutionSet {
        todo!()
    }
}

#[derive(Clone)]
pub struct Scope {
    lookup_context: ImplLookupContext,
}

#[derive(Copy, Clone)]
pub struct Candidate {
    uninferred_impl: UninferredImpl,
}
impl Candidate {
    fn generic_params(&self) -> Vec<GenericParam> {
        todo!()
    }

    fn goal(&self, args: &[GenericArgumentId]) -> Goal {
        todo!()
    }
}

#[derive(Copy, Clone)]
pub struct Solution {
    impl_id: ImplId,
}
impl Solution {
    fn solved_goal(&self) -> Goal {
        todo!()
    }
}

#[derive(Copy, Clone)]
pub enum SolutionSet {
    None,
    Unique(Solution),
    // TODO: Add info.
    Ambiguous,
}

#[derive(Default)]
pub struct Table;
impl Table {
    pub fn add_var(&self, generic_param: GenericParam) -> Var {
        todo!()
    }

    pub fn conform_goal(&self, goal0: Goal, goal1: Goal) -> Option<()> {
        todo!()
    }

    pub fn impl_vars(&self) -> Vec<ImplVar> {
        todo!()
    }

    pub fn impl_var_canonic_subgoal(&self, var: ImplVar) -> Goal {
        todo!()
    }
}
#[derive(Copy, Clone)]
pub enum Var {
    Type,
    Impl(ImplVar),
}
impl Var {
    fn arg(&self) -> GenericArgumentId {
        todo!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct ImplVar;

pub struct GoalSolver {
    goal: Goal,
    scope: Scope,
    candidate_solvers: Vec<CandidateSolver>,
}

impl GoalSolver {
    pub fn solved(ctx: Context, goal: Goal, scope: Scope) -> Self {
        let candidates = goal.find_candidates(ctx, &scope);
        let candidate_solvers = candidates
            .iter()
            .copied()
            .filter_map(|candidate| CandidateSolver::solved(ctx, &scope, goal, candidate))
            .collect();

        Self { goal, scope, candidate_solvers }
    }

    pub fn solution(&self) -> SolutionSet {
        let mut solution_set = SolutionSet::None;
        for candidate_solver in &self.candidate_solvers {
            let candidate_solution_set = candidate_solver.solution();
            match (solution_set, candidate_solution_set) {
                (SolutionSet::None, candidate_solution_set) => {
                    solution_set = candidate_solution_set
                }
                (_, SolutionSet::None) => {}
                _ => {
                    solution_set = SolutionSet::Ambiguous;
                }
            }
        }
        solution_set
    }
}

pub struct CandidateSolver {
    goal: Goal,
    candidate: Candidate,
    subgoal_solver: SubgoalSolver,
}
impl CandidateSolver {
    pub fn solved(
        ctx: Context,
        scope: &Scope,
        goal: Goal,
        candidate: Candidate,
    ) -> Option<CandidateSolver> {
        let mut subgoal_solver = SubgoalSolver::default();
        let vars = candidate
            .generic_params()
            .into_iter()
            .map(|generic_param| subgoal_solver.table.add_var(generic_param).arg())
            .collect_vec();
        subgoal_solver.table.conform_goal(candidate.goal(&vars), goal)?;

        for impl_var in subgoal_solver.table.impl_vars() {
            subgoal_solver.add(impl_var);
        }
        subgoal_solver.solve(ctx);

        let candidate_solver = CandidateSolver { goal, candidate, subgoal_solver };
        Some(candidate_solver)
    }

    fn solution(&self) -> SolutionSet {
        todo!()
    }
}

/// Solves a set of subgoals.
#[derive(Default)]
pub struct SubgoalSolver {
    table: Table,
    // Add Info / index.
    pending: VecDeque<ImplVar>,
    refuted: Vec<ImplVar>,
    solved: Vec<(ImplVar, Solution)>,
    ambiguous: Vec<ImplVar>,
}
impl SubgoalSolver {
    pub fn add(&mut self, var: ImplVar) {
        self.pending.push_back(var);
    }

    pub fn solve(&mut self, ctx: Context) {
        while let Some(impl_var) = self.pending.pop_front() {
            // Update the goal to the current table.
            let goal = self.table.impl_var_canonic_subgoal(impl_var);
            let solution_set = goal.fetch_solution_set(ctx);

            let solution = match solution_set {
                SolutionSet::None => {
                    self.refuted.push(impl_var);
                    // No need to continue.
                    return;
                }
                SolutionSet::Unique(solution) => solution,
                SolutionSet::Ambiguous => {
                    self.ambiguous.push(impl_var);
                    continue;
                }
            };
            self.solved.push((impl_var, solution));

            // Update table.
            self.table
                .conform_goal(goal, solution.solved_goal())
                .expect("Solution does not solve goal");

            // Update all subgoals after adding table restrictions.
            // Refuted subgoals will stay refuted
            // Pending are still pending.
            // Solved subgoals have already applied their solution to the table, so they still have
            // a solution. This solution may become more specific.
            // Ambiguous might not be ambiguous anymore. Move to pending.
            self.pending.extend(self.ambiguous.drain(..));
        }
    }

    pub fn solution(&self) -> SubgoalSolutionSet {
        if !self.refuted.is_empty() {
            return SubgoalSolutionSet::None;
        }
        if !self.ambiguous.is_empty() {
            return SubgoalSolutionSet::Ambiguous;
        }
        assert!(self.pending.is_empty(), "solution() called on an unsolved solver");
        SubgoalSolutionSet::Unique(self.solved.clone().into_iter().collect())
    }
}

#[derive(Clone)]
pub enum SubgoalSolutionSet {
    None,
    Unique(OrderedHashMap<ImplVar, Solution>),
    // TODO: Add info.
    Ambiguous,
}
