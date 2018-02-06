import * as React from 'react';
import { Header } from './Header'
import { Vol } from './Types'
import { ServerApi, ApiError } from './ServerApi'
import { MessageBubbleProps, MessageBubbleAction } from './MessageBubble'

export interface ShelterRotaProps {}
export interface ShelterRotaState { vols: Vol[]
                                  , reqInProgress: boolean
                                  , initialDataLoaded: boolean
                                  , errorMessage: MessageBubbleProps
                                  }

export class ShelterRota extends React.Component<ShelterRotaProps, ShelterRotaState> {
    constructor(props: ShelterRotaProps) {
      super(props)
      this.state = { vols: [], reqInProgress: true, errorMessage: new MessageBubbleProps(), initialDataLoaded: false }
      this.errorMessageAction = this.errorMessageAction.bind(this)
    }

    componentDidMount() {
      Promise.all([ServerApi.vols()])
        .then(results => {
          console.log(results);
          this.setState({ reqInProgress: false, vols: results[0], initialDataLoaded: true })
        })
        .catch(err => {
          let apiError = err as ApiError
          console.log(err)
          this.setState({ reqInProgress: false, errorMessage: this.state.errorMessage.setMessage({ header: apiError.error, body: apiError.message, position: 'under', icon: 'warning'}) })
        })
    }

    render() {
      return <div>
               <Header reqInProgress={this.state.reqInProgress}
                       errorMessage={this.state.errorMessage}
                       initialDataLoaded={this.state.initialDataLoaded}
                       vols={this.state.vols}
                       errorMessageAction={this.errorMessageAction}>
               </Header>
               <div className="container">
                 <p>Some stuff here...</p>
               </div>
             </div>
    }

    errorMessageAction(action: MessageBubbleAction) {
      this.setState({errorMessage: this.state.errorMessage.afterAction(action)})
    }
}