class: Workflow
inputs:
  infoo:
    type: File
    secondaryFiles:
      - .bai
  inbar:
    type:
      type: array
      items: File
    secondaryFiles:
      - .bai
  infoobar:
    type:
      type: array
      items:
        type: array
        items: File
    secondaryFiles:
      - .bai
outputs:
  outfoo:
    type: File
    secondaryFiles:
      - .bai
  outbar:
    type:
      type: array
      items: File
    secondaryFiles:
      - .bai
  outfoobar:
    type:
      type: array
      items:
        type: array
        items: File
    secondaryFiles:
      - .bai
