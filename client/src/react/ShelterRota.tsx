import * as React from 'react';
import { Header } from './Header'
import { Vol } from './Types'
import { ServerAPI } from './ServerAPI'

export interface ShelterRotaProps {}
export interface ShelterRotaState { vols: Vol[], reqInProgress: boolean, errorMessage: string | null }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
    constructor(props: ShelterRotaProps) {
      super(props)
      this.state = { vols: [], reqInProgress: true, errorMessage: null }
    }

    componentDidMount() {
      ServerAPI.vols()
        .then(vols => {console.log(vols); this.setState({reqInProgress: false, vols: vols})})
        .catch(err => {console.log(err); this.setState({reqInProgress: false, errorMessage: err})})
    }

    render() {
        return <div>
                 <Header reqInProgress={this.state.reqInProgress}
                         errorMessage={this.state.errorMessage}
                         vols={this.state.vols}>
                 </Header>
                 <div className="container">
                   <p>Some stuff here...</p>
                 </div>
               </div>
    }
}