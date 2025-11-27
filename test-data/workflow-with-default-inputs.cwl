class: Workflow
inputs:
  number:
    type: int
    default: 13
  flag:
    type: boolean
    default: true
  reverseflag:
    type: boolean
    default: false
  foo:
    type: string
    default: bar
  arr:
    type:
      type: array
      items: int
    default:
      - 1
      - 2
      - 3
outputs: []
