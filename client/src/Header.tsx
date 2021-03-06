import * as React from 'react';
import { Vol } from './Types'
import { MessageBubble, Message } from './MessageBubble'
import { ServerApi, ApiError } from './ServerApi'
import { pure } from './Util'

export interface HeaderProps { currentVol: Vol | null
                             , reqInProgress: boolean
                             , initialDataLoaded: boolean
                             , vols: Vol[]
                             , error: ApiError | null
                             , editingVolDetails: boolean
                             , active: boolean
                             , apiRequest: (req: Promise<any>) => void
                             , clearCurrentVol: () => void
                             , editCurrentVol: () => void
                             , setActive: (active: boolean) => void
                             , rotaKey: string | undefined
                             , rotaExists: boolean
                             }

export interface HeaderState { errorMessage: Message | null
                             }

export class Header extends React.Component<HeaderProps, HeaderState> {
  constructor(props: HeaderProps) {
    super(props)
    this.state = { errorMessage: null
                 }
  }

  componentWillReceiveProps(props: HeaderProps) {
    if(props.error != this.props.error) {
      this.setState(
        { errorMessage: props.error
                        ? { header: props.error.error
                          , body: props.error.message
                          , position: 'Under'
                          , icon: 'warning'
                          }
                        : null
        })
    }
  }

  clearCurrentVol() {
    this.props.apiRequest(
      ServerApi.clearCurrentVolId(this.props.rotaKey)
        .then(() => {
          this.props.clearCurrentVol()
        }))
  }

  render() {
    if(!this.props.currentVol) {
      return (
        <div className="header initial-data-loading">
          <NoCurrentVolHeaderButtons visible={this.props.rotaExists}
                                     active={this.props.active}
                                     setActive={this.props.setActive} />
          <StatusIcon reqInProgress={this.props.reqInProgress}
                      errorMessage={this.state.errorMessage}>
          </StatusIcon>
          <h1>Night Shelter Rota</h1>
        </div>
      )
    } else {
      return (
        <div className="header">
          <CurrentVolHeaderButtons editingVolDetails={this.props.editingVolDetails}
                                   currentVol={this.props.currentVol}
                                   clearCurrentVol={this.clearCurrentVol.bind(this)}
                                   editCurrentVol={this.props.editCurrentVol} />
          <StatusIcon reqInProgress={this.props.reqInProgress}
                      errorMessage={this.state.errorMessage}>
          </StatusIcon>
          <h1>Night Shelter Rota for</h1>  
          <div className="vol-name">{this.props.currentVol.name}</div>
        </div>
      )
    }
  }
}

const NoCurrentVolHeaderButtons = pure((props: { visible: boolean
                                               , active: boolean
                                               , setActive: (active: boolean) => void
                                               }) => {
  if(!props.visible) {
    return null
  }

  let buttons = [];
  if(props.active) {
      buttons.push(<button className={`ui button media-large-screen`}
                           onClick={e => props.setActive(false)}
                           key="deactv-lg">
                     <i className={`icon icon-history`}></i>
                     Show inactive volunteers
                   </button>)
      buttons.push(<button className={`ui button media-larger-screen media-medium-screen`}
                           onClick={e => props.setActive(false)}
                           key="deactv-med">
                     <i className={`icon icon-history`}></i>
                     Show inactive volunteers
                   </button>)
      buttons.push(<button className={`ui button mini icon media-small-screen`}
                           onClick={e => props.setActive(false)}
                           key="deactv-sm">
                     <i className={`icon icon-history`}></i>
                   </button>)
  } else {
      buttons.push(<button className={`ui button media-large-screen`}
                           onClick={e => props.setActive(true)}
                           key="actv-lg">
                     <i className={`icon icon-user`}></i>
                     Show active volunteers
                   </button>)
      buttons.push(<button className={`ui button media-larger-screen media-medium-screen`}
                           onClick={e => props.setActive(true)}
                           key="actv-med">
                     <i className={`icon icon-user`}></i>
                     Show active volunteers
                   </button>)
      buttons.push(<button className={`ui button mini icon media-small-screen`}
                           onClick={e => props.setActive(true)}
                           key="actv-sm">
                     <i className={`icon icon-user`}></i>
                   </button>)
  }
  
  return <div className="header-buttons">
           {buttons}
         </div>
})

const CurrentVolHeaderButtons = pure((props: { editingVolDetails: boolean 
                                             , currentVol: Vol | null
                                             , clearCurrentVol: () => void
                                             , editCurrentVol: () => void
                                             }) => {
  let buttons = [];
  if(!props.editingVolDetails) {
    buttons.push(<HeaderButton buttonClassName="header-button-vols"
                               mediaClassName="media-large-screen"
                               text="All volunteers"
                               icon="users"
                               action={props.clearCurrentVol}
                               key="vols-lg" />)
    buttons.push(<HeaderButton buttonClassName="header-button-vols"
                               mediaClassName="media-larger-screen media-medium-screen"
                               text="Vols"
                               icon="users"
                               action={props.clearCurrentVol}
                               key="vols-lgr-med" />)
    buttons.push(<HeaderButton buttonClassName="mini icon header-button-vols"
                               mediaClassName="media-small-screen"
                               text={null}
                               icon="users"
                               action={props.clearCurrentVol}
                               key="vols-sm" />)

    if(props.currentVol != null) {
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-large-screen"
                                 text="Edit your details"
                                 icon="edit"
                                 action={props.editCurrentVol}
                                 key="edit-lg" />)
      buttons.push(<HeaderButton buttonClassName="header-button-edit"
                                 mediaClassName="media-larger-screen media-medium-screen"
                                 text="Edit"
                                 icon="edit"
                                 action={props.editCurrentVol}
                                 key="edit-lgr-med" />)
      buttons.push(<HeaderButton buttonClassName="mini icon header-button-edit"
                                 mediaClassName="media-small-screen"
                                 text={null}
                                 icon="edit"
                                 action={props.editCurrentVol}
                                 key="edit-sm" />)
    }
  }
  
  return <div className="header-buttons">
           {buttons}
         </div>
})

const HeaderButton = pure((props: { buttonClassName: string
                                  , mediaClassName: string
                                  , text: string | null
                                  , icon: string
                                  , action: () => void
                                  }) => {
  return <button className={`ui button header-button ${props.buttonClassName} ${props.mediaClassName}`}
                 onClick={e => {e.preventDefault(); props.action(); }}>
           <i className={`icon icon-${props.icon}`}></i>
           {props.text}
         </button>
})

class StatusIcon extends React.Component<{ reqInProgress: boolean
                                         , errorMessage: Message | null
                                         }, {}> {
  messageBubble: MessageBubble | null

  render() {
    return <div className="header-status">
             <i className={this.iconType()}
                onClick={e => { e.preventDefault(); this.messageBubble && this.messageBubble.toggleFixed() }}
                onMouseOver={e => { e.preventDefault(); this.messageBubble && this.messageBubble.showTransitory() }}
                onMouseLeave={e => { e.preventDefault(); this.messageBubble && this.messageBubble.hideTransitory() }}>
             </i>
             <MessageBubble message={this.props.errorMessage}
                            otherFixedMessage={false}
                            messageFixedStateChanged={() => {}}
                            ref={messageBubble => {this.messageBubble = messageBubble}}>
             </MessageBubble>
          </div>
  }

  iconType() {
    if(this.props.reqInProgress) {
      return 'icon-spin animate-spin'
    }

    return this.props.errorMessage
             ? 'icon-warning'
             : 'logo'
  }
}