import * as React from 'react';
import { Vol } from './Types'

export interface RosterProps { visible: boolean
                             , currentVol: Vol | null
                             //, shifts: Shift[]
                             , currentDate: Date
                             }

export interface RosterState { startDate: Date
                            //  , endDate: Date
                             }

export class Roster extends React.Component<RosterProps, RosterState> {
  constructor(props: RosterProps) {
    super(props)
    this.state = { startDate: new Date(props.currentDate) }
  }
 
  render() {
    if(!this.props.visible) {
      return null
    }

    return <div onClick={this.changeStartDate.bind(this)}>
               Roster today: {this.props.currentDate.toString()}
               start: {this.state.startDate.toString()}
           </div>
  }

  changeStartDate() {
    let st = this.state.startDate
    let nst = new Date(st.setDate(st.getDay() + 1))
    this.setState({ startDate: nst })
  }
}
