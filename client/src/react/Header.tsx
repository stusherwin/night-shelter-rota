import * as React from 'react';
import { Vol } from './Types'
import { MessageBubble, MessageBubbleProps, MessageBubbleAction } from './MessageBubble'

export type VolDetailsState = 'NotEditing' 
                            | 'EditingNewVol'
                            | 'EditingCurrentVol'

export interface HeaderProps { reqInProgress: boolean
                             , errorMessage: MessageBubbleProps
                             , vols: Vol[]
                             , initialDataLoaded: boolean
                             , errorMessageAction: (action: MessageBubbleAction) => void
                             , changeCurrentVol: (vol: Vol | null) => void
                             , editCurrentVol: () => void
                             , editNewVol: () => void
                             }

export interface HeaderState { volDetailsState: VolDetailsState
                             , currentVol: Vol | null
                             }

export class Header extends React.Component<HeaderProps, HeaderState> {
  constructor(props: HeaderProps) {
    super(props)
    this.state = { volDetailsState: 'NotEditing', currentVol: null }
  }
 
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
               <HeaderButtons volDetailsState={this.state.volDetailsState}
                              currentVol={this.state.currentVol}
                              editNewVol={this.props.editNewVol}
                              editCurrentVol={this.props.editCurrentVol} />
               <StatusIcon reqInProgress={this.props.reqInProgress}
                           errorMessage={this.props.errorMessage}
                           action={this.props.errorMessageAction}>
               </StatusIcon>
               <h2>Night Shelter Rota for {this.props.vols[0].name}</h2>
             </div>
    }
  }
}

function HeaderButtons(props: { volDetailsState: VolDetailsState 
                              , currentVol: Vol | null
                              , editNewVol: () => void
                              , editCurrentVol: () => void
                              } ): JSX.Element {
  let buttons = [];
  if(props.volDetailsState == 'NotEditing') {
    buttons.push(<HeaderButton buttonClassName="header-button-new"
                               mediaClassName="media-large-screen"
                               text="New volunteer"
                               action={props.editNewVol} />)
    buttons.push(<HeaderButton buttonClassName="header-button-new"
                               mediaClassName="media-larger-screen media-medium-screen"
                               text="New"
                               action={props.editNewVol} />)
    buttons.push(<HeaderButton buttonClassName="header-button-new"
                               mediaClassName="media-small-screen"
                               text={null}
                               action={props.editNewVol} />)

    if(props.currentVol != null) {
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-large-screen"
                                 text="Edit volunteer details"
                                 action={props.editCurrentVol} />)
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-larger-screen media-medium-screen"
                                 text="Edit"
                                 action={props.editCurrentVol} />)
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-small-screen"
                                 text={null}
                                 action={props.editCurrentVol} />)
    }
  }
  
  return <div className="header-buttons">
           {buttons}
         </div>
}

function HeaderButton(props: { buttonClassName: string
                             , mediaClassName: string
                             , text: string | null
                             , action: () => void
                             } ): JSX.Element | null {
  return <button className={`ui button header-button ${props.buttonClassName} ${props.mediaClassName}`}
                 onClick={e => {e.preventDefault(); props.action(); }}>
           <i className="icon icon-add"></i>
           {props.text}
         </button>
}

class StatusIcon extends React.Component<{ reqInProgress: boolean
                                         , errorMessage: MessageBubbleProps
                                         , action: (action: MessageBubbleAction) => void
                                         }, {}> {
  render() {
    return <div className="header-status">
             <i className={this.iconType()}
                onClick={e => { e.preventDefault(); this.props.action('ToggleFixed'); }}
                onMouseOver={e => { e.preventDefault(); this.props.action('ShowTransitory'); }}
                onMouseLeave={e => { e.preventDefault(); this.props.action('HideTransitory'); }}>
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