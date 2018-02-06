import * as React from 'react';
import { Vol } from './Types'
import { MessageBubble, MessageBubbleProps, MessageBubbleAction } from './MessageBubble'

export interface HeaderProps { reqInProgress: boolean
                             , errorMessage: MessageBubbleProps
                             , vols: Vol[]
                             , initialDataLoaded: boolean
                             , errorMessageAction: (action: MessageBubbleAction) => void
                             }
export interface HeaderState {}

export class Header extends React.Component<HeaderProps, HeaderState> {
    render() {
      if(!this.props.initialDataLoaded) {
        return <div className="header initial-data-loading">
                 <StatusIcon reqInProgress={this.props.reqInProgress}
                             errorMessage={this.props.errorMessage}
                             action={this.props.errorMessageAction}>
                 </StatusIcon>
                 <h2>Night Shelter Rota</h2>
               </div>
      } else {
        return <div className="header">
                 <StatusIcon reqInProgress={this.props.reqInProgress}
                             errorMessage={this.props.errorMessage}
                             action={this.props.errorMessageAction}>
                 </StatusIcon>
                 <h2>Night Shelter Rota for {this.props.vols[0].name}</h2>
               </div>
      }
    }
}

class StatusIcon extends React.Component<{ reqInProgress: boolean
                                         , errorMessage: MessageBubbleProps
                                         , action: (action: MessageBubbleAction) => void
                                         }, {}> {
  render() {
    return <div className="header-status">
             <i className={this.iconType()}
                onClick={ e => { e.preventDefault(); this.props.action('ToggleFixed'); }}
                onMouseOver={ e => { e.preventDefault(); this.props.action('ShowTransitory'); }}
                onMouseLeave={ e => { e.preventDefault(); this.props.action('HideTransitory'); }}>
             </i>
             <MessageBubble {...this.props.errorMessage}
                            action={this.props.action}>
             </MessageBubble>
          </div>
  }

  iconType() {
    if(this.props.reqInProgress) {
      return 'icon-spin animate-spin'
    }

    return this.props.errorMessage.message
             ? 'icon-warning'
             : 'logo'
  }
}