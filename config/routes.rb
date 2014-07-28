# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

Verboice::Application.routes.draw do

  resources :channels do
    resources :queued_calls
    member do
      get :call
    end
  end

  resources :nuntium_channels, except: [:show]

  match '/' => 'home#index',  :as => 'home'

  devise_for :accounts, controllers: {omniauth_callbacks: "omniauth_callbacks"}
  guisso_for :account

  resources :feeds, controller: :feed_server do
    member do
      get :recordings
      get 'recording/:recording_id', action: :get_recording, as: :recording
    end
  end

  # Register both shallow and deep routes:
  # - Shallow routes allow for easier path helper methods, such as contact_recorded_audios(@contact) instead of project_contact_recorded_audios(@project, @contact)
  # - Deep routes ensure that form_for directives work as expected, so form_for([@project, @contact]) works no matter it is a creation or an update
  [true, false].each do |shallow|
    resources :projects, :shallow => shallow do
      member do
        post :enqueue_call
        put :update_variables
      end

      resources :call_flows, except: [:new, :edit] do
        member do
          get :edit_workflow, path: :designer
          put :update_workflow, path: :update_flow
          post :import
          get :export
          get :download_results
          get :oauth
        end
      end

      resources :external_services, except: [:show, :new, :edit] do
        member do
          put :update_manifest
        end
      end

      resources :schedules

      resources :contacts, except: [:show]

      resources :resources do
        collection do
          get :find
        end

        resources :localized_resources do
          member do
            post :save_recording
            get :play_recording
            post :save_file
            get :play_file
          end
        end
      end

      resources :feeds
    end
  end

  resources :call_logs, path: :calls do
    member do
      get :progress
      get 'results/:key', :action => :play_result, :as => 'result'
      get :download_details
    end
    collection do
      get :queued
      get :download
    end
  end

  resource :synthesizer do
    get :voices
  end

  resources :alerts do
    member do
      get :dismiss
    end
  end

  namespace :api do
    match "call" => "calls#call"
    resources :calls, only: [] do
      member do
        match :state
        match :redirect
      end
    end
    get "channels" => "channels#list"
    resources :channels, only: [:create] do
      collection do
        get ":name", :action => "get"
        put ":name", :action => "update"
        delete ":name", :action => "destroy"
      end
    end
    resources :projects, only: [:index] do
      resources :contacts do
        collection do
          get 'by_address/:address', :action => "show_by_address"
          put 'by_address/:address', :action => "update_by_address"
          put 'all', :action => "update_all"
        end
      end
      resources :schedules, only: [:index, :create] do
        collection do
          get ':name', :action => "show"
          put ':name', :action => "update"
          delete ':name', :action => "destroy"
        end
      end
    end
    resources :logs, only: [] do
      collection do
        get ':call_id', action: :list
      end
    end
  end

  get 'permissions' => 'permissions#index'
  get 'permissions/autocomplete' => 'permissions#autocomplete'
  post 'permissions/add_account' => 'permissions#add_account'
  post 'permissions/update' => 'permissions#update'

  post 'call_simulator/start'
  post 'call_simulator/resume'

  get 'oauth/google' => 'oauth#google', :as => 'google_oauth'
  match 'oauth/google/callback' => 'oauth#google_callback', :as => 'google_callback_oauth'

  root :to => 'home#index'

  get 'terms_and_conditions', :to => redirect('/')
end
