from agilerl.training.train_multi_agent_off_policy import train_multi_agent_off_policy
from agilerl.training.train_multi_agent_on_policy import train_multi_agent_on_policy
from agilerl.components.multi_agent_replay_buffer import MultiAgentReplayBuffer
from agilerl.algorithms import IPPO
from agilerl.utils.utils import create_population
from agilerl.vector.pz_async_vec_env import AsyncPettingZooVecEnv
from agilerl.algorithms.core.registry import HyperparameterConfig, RLParameter
from agilerl.hpo.mutation import Mutations
from agilerl.hpo.tournament import TournamentSelection
import sys

from haskboard_env import HaskboardEnv, make

sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]
device='cpu'
population_size=6

env = make("/home/adam/haskell/haskboard/dist-newstyle/build/x86_64-linux/ghc-9.10.1/NoMerci-0.1.0.0/x/NoMerci/build/NoMerci/NoMerci"
            ,shared=False)
_ = env.reset()

def get_memory(size:int =500,device:str ='cpu'):
    return MultiAgentReplayBuffer(memory_size=size,
field_names=["state", "action", "reward", "next_state", "done"],
                                  agent_ids = [f"player{i}_{i}" for i in range(3)],
                      device=device)

def get_algo():
    return IPPO( observation_spaces = env.observation_spaces, action_spaces = env.action_spaces,)

INIT_HP = {}
INIT_HP['AGENT_IDS'] = env.agents

NET_CONFIG = {agent: {
        "encoder_config": {"hidden_size": [32, 32], "activation": "ReLU"},
        "head_config": {"hidden_size": [32]},
    }
for agent in env.agents}

HP_CONFIG = HyperparameterConfig(
    lr = RLParameter(min=1e-4, max=1e-2),
    batch_size = RLParameter(min=8, max=1024),
    learn_step = RLParameter(min=256, max=8192, grow_factor=1.5, shrink_factor=0.75)
)

# TODO: more defaults
MUTATION_PARAMS = {
    # Relative probabilities
    'NO_MUT': 0.4,                              # No mutation
    'ARCH_MUT': 0.2,                            # Architecture mutation
    'NEW_LAYER': 0.2,                           # New layer mutation
    'PARAMS_MUT': 0.2,                          # Network parameters mutation
    'ACT_MUT': 0,                               # Activation layer mutation
    'RL_HP_MUT': 0.2,                           # Learning HP mutation
    'MUT_SD': 0.1,                              # Mutation strength
    'RAND_SEED': 1,                             # Random seed
}

# TODO: some default
mutations = Mutations(
    no_mutation=0.2,  # Probability of no mutation
    architecture=0.2,  # Probability of architecture mutation
    new_layer_prob=0.2,  # Probability of new layer mutation
    parameters=0.2,  # Probability of parameter mutation
    activation=0,  # Probability of activation function mutation
    rl_hp=0.2,  # Probability of RL hyperparameter mutation
    mutation_sd=0.1,  # Mutation strength
    device=device,
)

tournament = TournamentSelection(
        tournament_size=3,
        elitism=True,
        population_size=population_size,
        eval_loop=1
        )


pop = create_population(
        algo="IPPO",
        net_config=None,
        INIT_HP=INIT_HP,
        observation_space=env.observation_spaces,
        action_space=env.action_spaces,
        hp_config=HP_CONFIG,
        population_size=population_size,
        num_envs=1,
        device=device

        )

(trained_pop, fitnesses) = train_multi_agent_on_policy(
    env,
    env_name='NoMerci',  # Environment name
    algo="IPPO",  # Algorithm
    pop=pop,  # Population of agents
    sum_scores=False,
    INIT_HP=INIT_HP,
    MUT_P=MUTATION_PARAMS,
    max_steps=100000,  # Max number of training steps
    evo_steps=1000,  # Evolution frequency
    eval_steps=None,  # Number of steps in evaluation episode
    eval_loop=1,  # Number of evaluation episodes
    # target=-30.0,  # Target score for early stopping
    tournament=tournament,  # Tournament selection object
    mutation=mutations,  # Mutations object
    checkpoint_path="/home/adam/haskell/haskboard/python/runs/",
    checkpoint=1000
)

# (algo, fitnesses) =  train_multi_agent_on_policy(
#     env=env,
#     env_name="NoMerci",
#     algo="IPPO",
#     pop=pop,
#     memory=get_memory(),
#     sum_scores=False,
#     INIT_HP=None, # TODO: add hp
#     MUT_P=None, # TODO: add hp
#     max_steps=100,
#     evo_steps=25, # TODO: this is the default
#     eval_loop=1, # TODO: this is the default
#     tournament=None, # TODO: add tournament
#     mutation=None, # TODO: add mutation
#     checkpoint=50,
#     checkpoint_path="runs/checkpoints"
# )

# if __name__ == "__main__":
#     sys.stdout.reconfigure(line_buffering=True)  # type: ignore[union-attr]
#     main()
