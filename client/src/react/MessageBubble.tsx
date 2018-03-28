import * as React from 'react';

export type MessageBubbleAction = 'ToggleFixed'
                                | 'ShowTransitory'
                                | 'HideTransitory'

type MessageBubbleState = 'Hidden' | 'Fixed' | 'Transitory' | 'OtherFixedMessage'

export type Message = { header: string | null
                      , body: string
                      , icon: string | null
                      , position: MessageBubblePosition
                      }

export type MessageBubblePosition = 'Under' | 'Over'

export interface IMessageBubbleProps {
  message: Message | null
  state: MessageBubbleState
  action: (action: MessageBubbleAction) => void
}

export class MessageBubbleProps {
  constructor(message: Message | null = null, state: MessageBubbleState = 'Hidden' ) {
    this.message = message
    this.state = state
  }
  readonly message: Message | null
  readonly state: MessageBubbleState

  setMessage(message: Message | null): MessageBubbleProps {
    return new MessageBubbleProps(message, this.state)
  }

  showTransitory(): MessageBubbleProps {
    if(this.state == 'Hidden' && this.message != null) {
      return new MessageBubbleProps(this.message, 'Transitory')
    }
    return this
  }

  hideTransitory(): MessageBubbleProps {
    if(this.state == 'Transitory') {
      return new MessageBubbleProps(this.message, 'Hidden')
    }
    return this;
  }

  toggleFixed(): MessageBubbleProps {
    if(this.state == 'Hidden' && this.message != null) {
      return new MessageBubbleProps(this.message, 'Fixed')
    }
    if(this.state == 'Transitory') {
      return new MessageBubbleProps(this.message, 'Fixed')
    }
    if(this.state == 'Fixed') {
      return new MessageBubbleProps(this.message, 'Hidden')
    }
    if(this.state == 'OtherFixedMessage' && this.message != null) {
      return new MessageBubbleProps(this.message, 'Fixed')
    }
    return this;
  }

  otherFixedMessage(): MessageBubbleProps {
    return new MessageBubbleProps(this.message, 'OtherFixedMessage')
  }

  otherFixedMessageHidden(): MessageBubbleProps {
    if(this.state == 'OtherFixedMessage') {
      return new MessageBubbleProps(this.message, 'Hidden')
    }
    return this;
  }

  afterAction(action: MessageBubbleAction): MessageBubbleProps {
    switch(action) {
      case 'ShowTransitory':
        return this.showTransitory()
     
      case 'HideTransitory':
        return this.hideTransitory()
     
      case 'ToggleFixed':
        return this.toggleFixed()
    }

    return this;
  }
}

export class MessageBubble extends React.Component<IMessageBubbleProps, {}> {
  render() {
    if(this.props.state == 'Hidden' || this.props.message == null) {
      return null
    } else {
      return <div className={this.classNames()}
                  onClick={e => {e.preventDefault(); this.props.action('ToggleFixed'); }}>
               <CloseButton {...this.props} />
               <Header {...this.props} />
               <p>
                 <BodyIcon {...this.props} />
                 {this.props.message.body}
               </p>
             </div>
    }
  }

  classNames(): string {
    let result = ['message-bubble']

    if(this.props.message != null && this.props.message.position == 'Over') {
      result.push('inverted')
    }

    if(this.props.state == 'Fixed') {
      result.push('fixed')
    }

    return result.join(' ')
  }
}

function CloseButton(props: IMessageBubbleProps): JSX.Element | null {
  if(props.state != 'Fixed') {
    return null
  }

  return <a href="#" onClick={e => {e.preventDefault(); props.action('ToggleFixed'); }}>
           <i className="icon-cancel"></i>
         </a>
}

function Header(props: IMessageBubbleProps): JSX.Element | null {
  if(props.message == null || props.message.header == null) {
    return null
  }

  return <h3>
           <HeaderIcon {...props}></HeaderIcon>
           {props.message.header}
         </h3>
}

function HeaderIcon(props: IMessageBubbleProps): JSX.Element | null {
  if(props.message == null || props.message.icon == null) {
    return null
  }

  return <i className={`icon-${props.message.icon}`}></i>
}

function BodyIcon(props: IMessageBubbleProps): JSX.Element | null {
  if(props.message == null || props.message.icon == null) {
    return null
  }

  if(props.message.header != null) {
    return null
  }

  return <i className={`icon-${props.message.icon}`}></i>
}