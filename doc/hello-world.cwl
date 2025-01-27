cwlVersion: v1.2
class: CommandLineTool
baseCommand:
  - echo
  - Hello
inputs:
  name:
    type: string
    inputBinding:
      position: 1
outputs:
  message:
    type: stdout
