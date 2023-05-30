from enum import Enum


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

