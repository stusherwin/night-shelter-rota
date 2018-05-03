import * as React from 'react';
import { pure } from './Util'
import { Vol } from './Types'

export const CurrentVolSelector = pure((props: { visible: boolean
                                        , vols: Vol[]
                                        , apiRequest: (req: Promise<any>) => void
                                        , changeCurrentVol: (vol: Vol | null) => void
                                        }) => !props.visible ? null : (
  <div>
    Like, there isn't a current volunteer mate
  </div>
))