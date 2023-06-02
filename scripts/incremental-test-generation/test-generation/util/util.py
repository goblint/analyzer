from enum import Enum
import shutil


class Mutations:
    def __init__(self, rfb=False, uoi=False, ror=False, cr=False, rt=False, lcr=False):
        self.rfb = rfb
        self.rfb_s = "remove-function-body"
        self.uoi = uoi
        self.uoi_s = "unary-operator-inversion"
        self.ror = ror
        self.ror_s = "relational-operator-replacement"
        self.cr = cr
        self.cr_s = "constant-replacement"
        self.rt = rt
        self.rt_s = "remove-thread"
        self.lcr = lcr
        self.lcr_s = "logical-connector-replacement"

class Generate_Type(Enum):
    SOURCE = 'SOURCE'
    MUTATION = 'MUTATION'
    ML = 'ML'
    GIT = 'GIT'

SEPERATOR = "--------------------"

META_FILENAME = 'meta.yaml'
META_N = 'n'
META_COMPILING = 'compilation'
META_DIFF = 'diff'
META_TYPE = 'type'
META_SUB_TYPE = 'sub_type'
META_LINES = 'lines'

CONFIG_FILENAME = 'config.yaml'
CONFIG_GOBLINT = "goblint-path"
CONFIG_LLVM = "llvm-path"
CONFIG_LAST_INPUT_MUTATION = "last-input-mutation"
CONFIG_LAST_INPUT_GIT = "last-input-git"

APIKEY_FILENAME = 'api-key.yaml'
APIKEY_APIKEY = 'api-key'
APIKEY_ORGANISATION = 'organisation'

DEFAULT_ML_COUNT = 5

def make_program_copy(program_path, index):
    new_path = program_path.rsplit('.', 1)[0] + '_' + str(index) + '.c'
    shutil.copy2(program_path, new_path)
    return new_path
