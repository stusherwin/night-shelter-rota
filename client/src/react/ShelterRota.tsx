import * as React from 'react';
import { Header, HeaderProps } from './Header'
import { Roster, RosterProps } from './Roster'
import { Vol, ShiftType } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'
import { Util } from './Util'

export interface ShelterRotaProps {}
export interface ShelterRotaState { header: HeaderProps
                                  , roster: RosterProps
                                  }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
  constructor(props: ShelterRotaProps) {
    super(props)
    this.state = { header: { vols: []
                           , reqInProgress: true
                           , error: null
                           , initialDataLoaded: false
                           , changeCurrentVol: this.changeCurrentVol.bind(this)
                           , editNewVol: this.editNewVol.bind(this)
                           , editCurrentVol: this.editCurrentVol.bind(this)
                           }
                 , roster: { visible: false
                           , currentVol: null
                           , config: { maxVolsPerShift: 2
                                     , currentDate: Util.today()
                                     , urgentPeriodDays: 14
                                     }
                           , shifts: []
                           , addCurrentVol: this.addCurrentVol.bind(this)
                           , removeCurrentVol: this.removeCurrentVol.bind(this)
                           }
                 }
  }

  componentDidMount() {
    Promise.all([ServerApi.vols(), ServerApi.shifts()])
      .then(results => {
        console.log(results);
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , vols: results[0]
                                                                 , initialDataLoaded: true
                                                                 })
                      , roster: Object.assign(this.state.roster, { visible: true
                                                                 , shifts: results[1]
                                                                 })
                      })
      })
      .catch(err => {
        let apiError = err as ApiError
        console.log(err)
        this.setState({ header: Object.assign(this.state.header, { reqInProgress: false
                                                                 , error: apiError
                                                                 })
                      })
      })
  }

  render() {
    return (
      <div>
        <Header {...this.state.header} />
        <div className="container">
          <Roster {...this.state.roster} />
        </div>
      </div>
    )
  }

  changeCurrentVol(vol: Vol | null) {
    console.log('current vol: ' + (vol? vol.name : 'none'))
    this.setState({ roster: Object.assign(this.state.roster, { currentVol: vol
                                                             })
                  })
  }

  editNewVol() {
    console.log('edit new vol')
  }

  editCurrentVol() {
    console.log('edit current vol')
  }

  addCurrentVol(shiftDate: Date, shiftType: ShiftType) {
    console.log('add current vol')
  }

  removeCurrentVol(shiftDate: Date) {
    console.log('remove current vol')
  }
}